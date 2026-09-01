package net.ghoula.sarati.codec

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.xml.XmlNode

/** [[Decoder]] instances from [[XmlNode]] to primitives and collections.
  *
  * Scalars read the node's text content: a `Text`/`CData` node's own content, or an element's
  * concatenated text and CDATA children (whitespace included — attributes are never read here).
  * Numeric targets parse the trimmed text and fail on anything unparseable; booleans accept
  * `true`/`false`/`1`/`0`/`yes`/`no` case-insensitively. The `Option` decoder maps a childless
  * element and whitespace-only `Text` nodes to `None`; `List`/`Seq`/`Vector` read an element's
  * child elements; `Map` reads an `<object>` element's children keyed by `localName`.
  */
object XmlDecoders {

  private val defaultLoc: Location = (line = 1, column = 1, offset = 0)

  given Decoder[XmlNode, String] = new Decoder[XmlNode, String] {
    def decode(value: XmlNode): Result[DecodeError, String] = value match {
      case XmlNode.Text(content) => Result.Success(content, 0)
      case XmlNode.CData(content) => Result.Success(content, 0)
      case XmlNode.Element(_, _, children) =>
        val textContent = children.collect {
          case XmlNode.Text(t) => t
          case XmlNode.CData(c) => c
        }.mkString
        Result.Success(textContent, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("String", xmlNodeTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[XmlNode, Int] = new Decoder[XmlNode, Int] {
    def decode(value: XmlNode): Result[DecodeError, Int] =
      getTextContent(value) match {
        case Result.Success(text, _) =>
          text.trim.toIntOption match {
            case Some(n) => Result.Success(n, 0)
            case None =>
              Result.Failure(
                List(DecodeError.TypeMismatch("Int", s"'$text' is not a valid integer", defaultLoc)),
                defaultLoc
              )
          }
        case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        case Result.Partial(_, errors, _) => Result.Failure(errors, defaultLoc)
      }
  }

  given Decoder[XmlNode, Long] = new Decoder[XmlNode, Long] {
    def decode(value: XmlNode): Result[DecodeError, Long] =
      getTextContent(value) match {
        case Result.Success(text, _) =>
          text.trim.toLongOption match {
            case Some(n) => Result.Success(n, 0)
            case None =>
              Result.Failure(
                List(DecodeError.TypeMismatch("Long", s"'$text' is not a valid long", defaultLoc)),
                defaultLoc
              )
          }
        case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        case Result.Partial(_, errors, _) => Result.Failure(errors, defaultLoc)
      }
  }

  given Decoder[XmlNode, Double] = new Decoder[XmlNode, Double] {
    def decode(value: XmlNode): Result[DecodeError, Double] =
      getTextContent(value) match {
        case Result.Success(text, _) =>
          text.trim.toDoubleOption match {
            case Some(n) => Result.Success(n, 0)
            case None =>
              Result.Failure(
                List(DecodeError.TypeMismatch("Double", s"'$text' is not a valid double", defaultLoc)),
                defaultLoc
              )
          }
        case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        case Result.Partial(_, errors, _) => Result.Failure(errors, defaultLoc)
      }
  }

  given Decoder[XmlNode, Float] = new Decoder[XmlNode, Float] {
    def decode(value: XmlNode): Result[DecodeError, Float] =
      getTextContent(value) match {
        case Result.Success(text, _) =>
          text.trim.toFloatOption match {
            case Some(n) => Result.Success(n, 0)
            case None =>
              Result.Failure(
                List(DecodeError.TypeMismatch("Float", s"'$text' is not a valid float", defaultLoc)),
                defaultLoc
              )
          }
        case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        case Result.Partial(_, errors, _) => Result.Failure(errors, defaultLoc)
      }
  }

  given Decoder[XmlNode, Boolean] = new Decoder[XmlNode, Boolean] {
    def decode(value: XmlNode): Result[DecodeError, Boolean] =
      getTextContent(value) match {
        case Result.Success(text, _) =>
          text.trim.toLowerCase match {
            case "true" | "1" | "yes" => Result.Success(true, 0)
            case "false" | "0" | "no" => Result.Success(false, 0)
            case _ =>
              Result.Failure(
                List(
                  DecodeError
                    .TypeMismatch("Boolean", s"'$text' is not a valid boolean", defaultLoc)
                ),
                defaultLoc
              )
          }
        case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        case Result.Partial(_, errors, _) => Result.Failure(errors, defaultLoc)
      }
  }

  given [A] => (decoder: Decoder[XmlNode, A]) => Decoder[XmlNode, Option[A]] =
    new Decoder[XmlNode, Option[A]] {
      def decode(value: XmlNode): Result[DecodeError, Option[A]] = decodeEval(value).run()

      override def decodeEval(value: XmlNode): Eval[Result[DecodeError, Option[A]]] =
        Eval.defer {
          value match {
            case XmlNode.Element(_, _, children) if children.isEmpty =>
              Eval.now(Result.Success(None, 0))
            case XmlNode.Text(content) if content.trim.isEmpty =>
              Eval.now(Result.Success(None, 0))
            case other =>
              decoder.decodeEval(other).map {
                case Result.Success(a, consumed) => Result.Success(Some(a), consumed)
                case Result.Partial(a, errors, consumed) => Result.Partial(Some(a), errors, consumed)
                case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
              }
          }
        }
    }

  given [A] => (decoder: Decoder[XmlNode, A]) => Decoder[XmlNode, List[A]] =
    new Decoder[XmlNode, List[A]] {
      def decode(value: XmlNode): Result[DecodeError, List[A]] = decodeEval(value).run()

      override def decodeEval(value: XmlNode): Eval[Result[DecodeError, List[A]]] =
        Eval.defer {
          value match {
            case XmlNode.Element(_, _, children) =>
              val elements = children.collect { case e: XmlNode.Element => e }
              Decoder.drainElements[XmlNode, A, List[A], List[A]](
                elements,
                decoder.decodeEval,
                emptyAcc = Nil,
                step = (_, a, es, acc, errs) => (a :: acc, es.reverse ::: errs),
                finish = (acc, errs) =>
                  if errs.isEmpty then Result.Success(acc.reverse, 0)
                  else Result.Partial(acc.reverse, errs.reverse, 0),
                onFail = es => Result.Failure(es, defaultLoc)
              )
            case other =>
              Eval.now(
                Result.Failure(
                  List(DecodeError.TypeMismatch("Element", xmlNodeTypeName(other), defaultLoc)),
                  defaultLoc
                )
              )
          }
        }
    }

  given [A] => (decoder: Decoder[XmlNode, A]) => Decoder[XmlNode, Seq[A]] =
    new Decoder[XmlNode, Seq[A]] {
      def decode(value: XmlNode): Result[DecodeError, Seq[A]] = decodeEval(value).run()

      override def decodeEval(value: XmlNode): Eval[Result[DecodeError, Seq[A]]] =
        summon[Decoder[XmlNode, List[A]]].decodeEval(value).map {
          case Result.Success(list, consumed) => Result.Success(list.toSeq, consumed)
          case Result.Partial(list, errors, consumed) => Result.Partial(list.toSeq, errors, consumed)
          case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        }
    }

  given [A] => (decoder: Decoder[XmlNode, A]) => Decoder[XmlNode, Vector[A]] =
    new Decoder[XmlNode, Vector[A]] {
      def decode(value: XmlNode): Result[DecodeError, Vector[A]] = decodeEval(value).run()

      override def decodeEval(value: XmlNode): Eval[Result[DecodeError, Vector[A]]] =
        summon[Decoder[XmlNode, List[A]]].decodeEval(value).map {
          case Result.Success(list, consumed) => Result.Success(list.toVector, consumed)
          case Result.Partial(list, errors, consumed) => Result.Partial(list.toVector, errors, consumed)
          case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        }
    }

  def getAttribute(element: XmlNode.Element, name: String): Option[String] =
    element.attributes.find(_.name.localName == name).map(_.value)

  def getChild(element: XmlNode.Element, name: String): Option[XmlNode.Element] =
    element.children.collectFirst {
      case e: XmlNode.Element if e.name.localName == name => e
    }

  def getChildren(element: XmlNode.Element, name: String): List[XmlNode.Element] =
    element.children.collect {
      case e: XmlNode.Element if e.name.localName == name => e
    }

  private def getTextContent(value: XmlNode): Result[DecodeError, String] = value match {
    case XmlNode.Text(content) => Result.Success(content, 0)
    case XmlNode.CData(content) => Result.Success(content, 0)
    case XmlNode.Element(_, _, children) =>
      val textContent = children.collect {
        case XmlNode.Text(t) => t
        case XmlNode.CData(c) => c
      }.mkString
      Result.Success(textContent, 0)
    case other =>
      Result.Failure(
        List(DecodeError.TypeMismatch("text content", xmlNodeTypeName(other), defaultLoc)),
        defaultLoc
      )
  }

  /** Reads an `<object>`-shaped element: each child element's `localName` is the key, the child
    * itself is the value — the shape [[XmlEncoders]]' map encoder and [[AstBuilder]] produce.
    * Duplicate keys: the later element wins (map semantics).
    */
  given [A] => (decoder: Decoder[XmlNode, A]) => Decoder[XmlNode, Map[String, A]] =
    new Decoder[XmlNode, Map[String, A]] {
      def decode(value: XmlNode): Result[DecodeError, Map[String, A]] = decodeEval(value).run()

      override def decodeEval(value: XmlNode): Eval[Result[DecodeError, Map[String, A]]] =
        Eval.defer {
          value match {
            case XmlNode.Element(_, _, children) =>
              val entries = children.collect { case e: XmlNode.Element => (e.name.localName, e) }
              Decoder.drainElements[(String, XmlNode), A, Map[String, A], Map[String, A]](
                entries,
                decodeElem = { case (_, fieldValue) => decoder.decodeEval(fieldValue) },
                emptyAcc = Map.empty[String, A],
                step = (kv, a, es, acc, errs) => (acc + (kv._1 -> a), es.reverse ::: errs),
                finish = (acc, errs) =>
                  errs match {
                    case Nil => Result.Success(acc, 0)
                    case nonEmpty => Result.Partial(acc, nonEmpty.reverse, 0)
                  },
                onFail = es => Result.Failure(es, defaultLoc)
              )
            case other =>
              Eval.now(
                Result.Failure(
                  List(DecodeError.TypeMismatch("Element", xmlNodeTypeName(other), defaultLoc)),
                  defaultLoc
                )
              )
          }
        }
    }

  private def xmlNodeTypeName(value: XmlNode): String = value match {
    case XmlNode.Element(name, _, _) => s"Element(${name.localName})"
    case XmlNode.Text(_) => "Text"
    case XmlNode.CData(_) => "CData"
    case XmlNode.Comment(_) => "Comment"
    case XmlNode.ProcessingInstruction(_, _) => "ProcessingInstruction"
  }
}
