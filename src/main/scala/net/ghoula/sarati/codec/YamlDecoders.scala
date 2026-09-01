package net.ghoula.sarati.codec

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.yaml.YamlValue

/** [[Decoder]] instances from [[YamlValue]] to primitives and collections.
  *
  * Unlike the JSON and TOML layers, the `String` decoder is permissive — every scalar kind renders
  * to text (`Null` as `"null"`) — while numeric and boolean targets stay strict to their value
  * kinds.
  */
object YamlDecoders {

  private val defaultLoc: Location = (line = 1, column = 1, offset = 0)

  given Decoder[YamlValue, String] = new Decoder[YamlValue, String] {
    def decode(value: YamlValue): Result[DecodeError, String] = value match {
      case YamlValue.String(s) => Result.Success(s, 0)
      case YamlValue.Integer(n) => Result.Success(n.toString, 0)
      case YamlValue.Float(n) => Result.Success(n.toString, 0)
      case YamlValue.Boolean(b) => Result.Success(b.toString, 0)
      case YamlValue.Null => Result.Success("null", 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("String", yamlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[YamlValue, Int] = new Decoder[YamlValue, Int] {
    def decode(value: YamlValue): Result[DecodeError, Int] = value match {
      case YamlValue.Integer(n) if n >= Int.MinValue && n <= Int.MaxValue =>
        Result.Success(n.toInt, 0)
      case YamlValue.Integer(n) =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Int", s"Integer($n) - out of range", defaultLoc)),
          defaultLoc
        )
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Int", yamlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[YamlValue, Long] = new Decoder[YamlValue, Long] {
    def decode(value: YamlValue): Result[DecodeError, Long] = value match {
      case YamlValue.Integer(n) => Result.Success(n, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Long", yamlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[YamlValue, Double] = new Decoder[YamlValue, Double] {
    def decode(value: YamlValue): Result[DecodeError, Double] = value match {
      case YamlValue.Float(n) => Result.Success(n, 0)
      case YamlValue.Integer(n) => Result.Success(n.toDouble, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Double", yamlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[YamlValue, Float] = new Decoder[YamlValue, Float] {
    def decode(value: YamlValue): Result[DecodeError, Float] = value match {
      case YamlValue.Float(n) => Result.Success(n.toFloat, 0)
      case YamlValue.Integer(n) => Result.Success(n.toFloat, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Float", yamlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given Decoder[YamlValue, Boolean] = new Decoder[YamlValue, Boolean] {
    def decode(value: YamlValue): Result[DecodeError, Boolean] = value match {
      case YamlValue.Boolean(b) => Result.Success(b, 0)
      case other =>
        Result.Failure(
          List(DecodeError.TypeMismatch("Boolean", yamlValueTypeName(other), defaultLoc)),
          defaultLoc
        )
    }
  }

  given [A] => (decoder: Decoder[YamlValue, A]) => Decoder[YamlValue, Option[A]] =
    new Decoder[YamlValue, Option[A]] {
      def decode(value: YamlValue): Result[DecodeError, Option[A]] = decodeEval(value).run()

      override def decodeEval(value: YamlValue): Eval[Result[DecodeError, Option[A]]] =
        Eval.defer {
          value match {
            case YamlValue.Null => Eval.now(Result.Success(None, 0))
            case other =>
              decoder.decodeEval(other).map {
                case Result.Success(a, consumed) => Result.Success(Some(a), consumed)
                case Result.Partial(a, errors, consumed) => Result.Partial(Some(a), errors, consumed)
                case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
              }
          }
        }
    }

  given [A] => (decoder: Decoder[YamlValue, A]) => Decoder[YamlValue, List[A]] =
    new Decoder[YamlValue, List[A]] {
      def decode(value: YamlValue): Result[DecodeError, List[A]] = decodeEval(value).run()

      override def decodeEval(value: YamlValue): Eval[Result[DecodeError, List[A]]] =
        Eval.defer {
          value match {
            case YamlValue.Sequence(elements) =>
              Decoder.drainElements[YamlValue, A, List[A], List[A]](
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
                  List(DecodeError.TypeMismatch("Sequence", yamlValueTypeName(other), defaultLoc)),
                  defaultLoc
                )
              )
          }
        }
    }

  given [A] => (decoder: Decoder[YamlValue, A]) => Decoder[YamlValue, Seq[A]] =
    new Decoder[YamlValue, Seq[A]] {
      def decode(value: YamlValue): Result[DecodeError, Seq[A]] = decodeEval(value).run()

      override def decodeEval(value: YamlValue): Eval[Result[DecodeError, Seq[A]]] =
        summon[Decoder[YamlValue, List[A]]].decodeEval(value).map {
          case Result.Success(list, consumed) => Result.Success(list.toSeq, consumed)
          case Result.Partial(list, errors, consumed) => Result.Partial(list.toSeq, errors, consumed)
          case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        }
    }

  given [A] => (decoder: Decoder[YamlValue, A]) => Decoder[YamlValue, Vector[A]] =
    new Decoder[YamlValue, Vector[A]] {
      def decode(value: YamlValue): Result[DecodeError, Vector[A]] = decodeEval(value).run()

      override def decodeEval(value: YamlValue): Eval[Result[DecodeError, Vector[A]]] =
        summon[Decoder[YamlValue, List[A]]].decodeEval(value).map {
          case Result.Success(list, consumed) => Result.Success(list.toVector, consumed)
          case Result.Partial(list, errors, consumed) => Result.Partial(list.toVector, errors, consumed)
          case Result.Failure(errors, furthest) => Result.Failure(errors, furthest)
        }
    }

  given [A] => (decoder: Decoder[YamlValue, A]) => Decoder[YamlValue, Map[String, A]] =
    new Decoder[YamlValue, Map[String, A]] {
      def decode(value: YamlValue): Result[DecodeError, Map[String, A]] = decodeEval(value).run()

      override def decodeEval(value: YamlValue): Eval[Result[DecodeError, Map[String, A]]] =
        Eval.defer {
          value match {
            case YamlValue.Mapping(pairs) =>
              Decoder.drainElements[(String, YamlValue), A, Map[String, A], Map[String, A]](
                pairs.toList,
                decodeElem = { case (_, fieldValue) => decoder.decodeEval(fieldValue) },
                emptyAcc = Map.empty[String, A],
                step = (kv, a, es, acc, errs) => (acc + (kv._1 -> a), es.reverse ::: errs),
                finish = (acc, errs) =>
                  if errs.isEmpty then Result.Success(acc, 0)
                  else Result.Partial(acc, errs.reverse, 0),
                onFail = es => Result.Failure(es, defaultLoc)
              )
            case other =>
              Eval.now(
                Result.Failure(
                  List(DecodeError.TypeMismatch("Mapping", yamlValueTypeName(other), defaultLoc)),
                  defaultLoc
                )
              )
          }
        }
    }

  private def yamlValueTypeName(value: YamlValue): String = value match {
    case YamlValue.Null => "Null"
    case YamlValue.Boolean(_) => "Boolean"
    case YamlValue.Integer(_) => "Integer"
    case YamlValue.Float(_) => "Float"
    case YamlValue.String(_) => "String"
    case YamlValue.Sequence(_) => "Sequence"
    case YamlValue.Mapping(_) => "Mapping"
  }
}
