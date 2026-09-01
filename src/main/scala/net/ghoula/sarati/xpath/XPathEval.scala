package net.ghoula.sarati.xpath

import net.ghoula.sarati.ast.xml.*

object XPathEval {

  // ==========================================================================
  // String values
  // ==========================================================================

  /** Concatenation of all descendant text (CDATA folded in) in document order. Recursion depth
    * equals document depth — the same bound as Spark's own DOM.
    */
  def descendantText(node: XNode): String = node match {
    case t: TextNode => t.text
    case _ => node.children.map(descendantText).mkString
  }

  /** The XPath 1.0 string-value of a node: text and CDATA as-is, attribute values, comment and PI
    * content (no target), and for elements and the document the concatenation of all descendant
    * text ([[descendantText]]).
    */
  def stringValue(node: XNode): String = node match {
    case _: Document => descendantText(node)
    case _: ElementNode => descendantText(node)
    case a: AttributeNode => a.value
    case t: TextNode => t.text
    case c: CommentNode => c.text
    case p: PiNode => p.content
  }

  // ==========================================================================
  // Entry points
  // ==========================================================================

  /** Entry point: evaluates `expr` against a parsed document. The context node is the document node
    * itself, with proximity position 1 and context size 1.
    */
  def eval(expr: XPathExpr, doc: XmlDocument): Either[XPathError, XPathValue] = {
    val document = wrapDocument(doc)
    eval(expr, Context(document, 1, 1))
  }

  /** Entry point: evaluates `expr` in the given [[Context]], for callers holding a document already
    * wrapped with [[wrapDocument]].
    */
  def eval(expr: XPathExpr, context: Context): Either[XPathError, XPathValue] =
    evalExpr(expr, context)

  // ==========================================================================
  // Expression evaluation
  // ==========================================================================

  private def evalExpr(expr: XPathExpr, ctx: Context): Either[XPathError, XPathValue] =
    expr match {
      case XPathExpr.Or(l, r) =>
        evalExpr(l, ctx) match {
          case Left(e) => Left(e)
          case Right(lv) =>
            if toBool(lv) then Right(XPathValue.Bool(true))
            else evalExpr(r, ctx).map(rv => XPathValue.Bool(toBool(rv)))
        }

      case XPathExpr.And(l, r) =>
        evalExpr(l, ctx) match {
          case Left(e) => Left(e)
          case Right(lv) =>
            if !toBool(lv) then Right(XPathValue.Bool(false))
            else evalExpr(r, ctx).map(rv => XPathValue.Bool(toBool(rv)))
        }

      case XPathExpr.Comparison(l, r, op) =>
        for {
          lv <- evalExpr(l, ctx)
          rv <- evalExpr(r, ctx)
          res <- compare(lv, rv, op)
        } yield res

      case XPathExpr.Arithmetic(l, r, op) =>
        for {
          lv <- evalExpr(l, ctx)
          rv <- evalExpr(r, ctx)
        } yield XPathValue.Number(applyArith(toNumberOrNaN(lv), toNumberOrNaN(rv), op))

      case XPathExpr.Negation(inner) =>
        evalExpr(inner, ctx).map(v => XPathValue.Number(-toNumberOrNaN(v)))

      case XPathExpr.Union(l, r) =>
        for {
          lv <- evalExpr(l, ctx)
          rv <- evalExpr(r, ctx)
          res <- (lv, rv) match {
            case (XPathValue.NodeSet(a), XPathValue.NodeSet(b)) =>
              Right(XPathValue.NodeSet(mergeDocumentOrder(a, b)))
            case _ => Left(XPathError.Invalid("union operands must be node-sets"))
          }
        } yield res

      case XPathExpr.Path(isAbsolute, steps) =>
        if isAbsolute then {
          val doc = ctx.node.doc
          if steps.isEmpty then Right(XPathValue.NodeSet(List(doc)))
          else
            evalSteps(steps, Context(doc, 1, 1), List(doc)).map { nodes =>
              XPathValue.NodeSet(sortDocumentOrder(nodes))
            }
        } else {
          evalSteps(steps, ctx, List(ctx.node)).map { nodes =>
            XPathValue.NodeSet(sortDocumentOrder(nodes))
          }
        }

      case XPathExpr.Filter(primary, predicates, steps) =>
        for {
          pv <- evalExpr(primary, ctx)
          startNodes <- pv match {
            case XPathValue.NodeSet(nodes) => Right(nodes)
            // the primary's node-set is document-ordered by construction, so predicate
            // positions run in document order per XPath 1.0 §2.4/§3.3
            case _ if predicates.nonEmpty =>
              Left(XPathError.Invalid("filter predicates require a node-set primary"))
            case _ =>
              if steps.isEmpty then Right(List.empty)
              else Left(XPathError.Invalid("location steps require a node-set primary"))
          }
          filtered <-
            if predicates.isEmpty then Right(startNodes)
            else applyPredicates(startNodes, predicates)
          out <-
            if steps.isEmpty then Right(filtered)
            else evalSteps(steps, Context(ctx.node, ctx.position, ctx.size), filtered)
        } yield XPathValue.NodeSet(sortDocumentOrder(out))

      case XPathExpr.Literal(v) => Right(XPathValue.Str(v))
      case XPathExpr.Number(v) => Right(XPathValue.Number(v))
      case XPathExpr.VariableRef(n) => Left(XPathError.Unsupported(s"variable references ($n)"))
      case XPathExpr.FunctionCall(name, args) => evalFunction(name, args, ctx)
    }

  /** Multi-step evaluation: each step applies to every node the previous steps produced. Step
    * results merge in document order.
    */
  private def evalSteps(
    steps: List[Step],
    ctx: Context,
    start: List[XNode]
  ): Either[XPathError, List[XNode]] =
    steps match {
      case Nil => Right(start)
      case step :: rest =>
        val outcomes = start.map(n => evalStep(step, n))
        outcomes.find(_.isLeft) match {
          case Some(Left(e)) => Left(e)
          case _ =>
            val nodeLists = outcomes.collect { case Right(ns) => ns }
            val merged = nodeLists.foldLeft(List.empty[XNode])((acc, l) => mergeDocumentOrder(acc, l))
            evalSteps(rest, ctx, merged)
        }
    }

  /** One step from a single context node: axis walk (in proximity order), node-test filter,
    * predicates applied with per-node positions. The namespace axis returns empty (documented
    * divergence).
    */
  private def evalStep(step: Step, node: XNode): Either[XPathError, List[XNode]] =
    step.axis match {
      case Axis.Namespace => Right(List.empty)
      case axis =>
        val axisNodes = walkAxis(axis, node)
        val tested = axisNodes.filter(matchesTest(_, step.test))
        val proximity = if Axis.isReverse(axis) then tested.reverse else tested
        applyPredicates(proximity, step.predicates)
    }

  private def applyPredicates(
    nodes: List[XNode],
    predicates: List[XPathExpr]
  ): Either[XPathError, List[XNode]] =
    predicates match {
      case Nil => Right(nodes)
      case p :: rest =>
        val size = nodes.size
        val kept = scala.collection.mutable.ListBuffer.empty[XNode]
        var idx = 0
        var failure: Option[XPathError] = None
        val it = nodes.iterator
        while it.hasNext && failure.isEmpty do {
          val n = it.next()
          idx += 1
          evalExpr(p, Context(n, idx, size)) match {
            case Left(e) => failure = Some(e)
            case Right(value) =>
              val keep = value match {
                case XPathValue.Number(d) => idx.toDouble == d // numeric predicate = position test
                case other => toBool(other)
              }
              if keep then kept += n
          }
        }
        failure match {
          case Some(e) => Left(e)
          case None => applyPredicates(kept.toList, rest)
        }
    }

  /** Axis walk. Forward axes return nodes in document order; reverse axes in reverse document order
    * (predicate proximity positions follow).
    */
  private[xpath] def walkAxis(axis: Axis, node: XNode): List[XNode] = {
    val doc = node.doc
    val all = doc.allNodes.toList
    axis match {
      case Axis.Child => node.children
      case Axis.Descendant =>
        all.filter(n => n.order > node.order && isDescendantOf(n, node))
      case Axis.Parent => node.parent.toList
      case Axis.Ancestor => {
        var cur = node.parent
        val acc = scala.collection.mutable.ListBuffer.empty[XNode]
        while cur.isDefined do {
          acc += cur.get
          cur = cur.get.parent
        }
        acc.toList
      }
      case Axis.FollowingSibling =>
        node.parent.toList
          .flatMap(p => p.children)
          .dropWhile(_ ne node)
          .drop(1)
      case Axis.PrecedingSibling =>
        node.parent.toList
          .flatMap(p => p.children)
          .takeWhile(_ ne node)
          .reverse
      case Axis.Following =>
        all.flatMap {
          case _: AttributeNode => None
          case n if n.order > node.order && !isDescendantOf(n, node) && !isAncestorOf(n, node) =>
            Some(n)
          case _ => None
        }
      case Axis.Preceding =>
        all.flatMap {
          case _: AttributeNode => None
          case n if n.order < node.order && !isAncestorOf(n, node) => Some(n)
          case _ => None
        }.reverse
      case Axis.Attribute =>
        node match {
          case e: ElementNode => e.attributes
          case _ => List.empty
        }
      case Axis.Namespace => List.empty
      case Axis.Self => List(node)
      case Axis.DescendantOrSelf =>
        node :: all.filter(n => n.order > node.order && isDescendantOf(n, node))
      case Axis.AncestorOrSelf => {
        var cur: Option[XNode] = Some(node)
        val acc = scala.collection.mutable.ListBuffer.empty[XNode]
        while cur.isDefined do {
          acc += cur.get
          cur = cur.get.parent
        }
        acc.toList
      }
    }
  }

  private def isDescendantOf(n: XNode, ancestor: XNode): Boolean = {
    var cur = n.parent
    var found = false
    while cur.isDefined && !found do {
      if cur.get eq ancestor then found = true
      else cur = cur.get.parent
    }
    found
  }

  private def isAncestorOf(n: XNode, maybeDescendant: XNode): Boolean =
    isDescendantOf(maybeDescendant, n)

  private def sortDocumentOrder(nodes: List[XNode]): List[XNode] =
    nodes.sortBy(_.order)

  private def mergeDocumentOrder(a: List[XNode], b: List[XNode]): List[XNode] =
    (a ++ b).sortBy(_.order).distinctBy(_.order)

  private def matchesTest(node: XNode, test: NodeTest): Boolean = (node, test) match {
    case (_, NodeTest.Node) => true
    case (t: TextNode, NodeTest.Text) => !t.cdata
    case (_: CommentNode, NodeTest.Comment) => true
    case (p: PiNode, NodeTest.ProcessingInstruction(nameOpt)) =>
      nameOpt.forall(_ == p.target)
    case (e: ElementNode, NodeTest.Name(nameTest)) => matchesName(e.name, nameTest)
    case (a: AttributeNode, NodeTest.Name(nameTest)) => matchesName(a.name, nameTest)
    case _ => false
  }

  private def matchesName(q: QName, test: NameTest): Boolean =
    test match {
      case NameTest.Any => true
      case NameTest.AnyLocalInNamespace(prefix) => q.prefix.contains(prefix)
      case NameTest.AnyNamespace(local) => q.localName == local
      case NameTest.Named(prefix, local) =>
        q.localName == local && q.prefix == prefix
    }

  // ==========================================================================
  // Operators
  // ==========================================================================

  private def compare(l: XPathValue, r: XPathValue, op: BinaryOp): Either[XPathError, XPathValue] = {
    def eqOf(a: XPathValue, b: XPathValue): Boolean = (a, b) match {
      case (XPathValue.NodeSet(ns), other) =>
        ns.exists(n => primitiveEq(XPathValue.Str(stringValue(n)), other))
      case (other, XPathValue.NodeSet(ns)) =>
        ns.exists(n => primitiveEq(other, XPathValue.Str(stringValue(n))))
      case (a, b) => primitiveEq(a, b)
    }
    def relOf(a: XPathValue, b: XPathValue): Boolean =
      numberCandidates(a).exists(x => numberCandidates(b).exists(y => applyRel(x, y, op)))
    val result = op match {
      case BinaryOp.Eq => eqOf(l, r)
      case BinaryOp.Ne => !eqOf(l, r)
      case _ => relOf(l, r)
    }
    Right(XPathValue.Bool(result))
  }

  private def primitiveEq(a: XPathValue, b: XPathValue): Boolean = (a, b) match {
    case (XPathValue.Bool(x), _) => x == toBool(b)
    case (_, XPathValue.Bool(y)) => toBool(a) == y
    case (XPathValue.Number(x), _) => x == toNumberOrNaN(b)
    case (_, XPathValue.Number(y)) => toNumberOrNaN(a) == y
    case (XPathValue.Str(x), _) => x == asString(b)
    case (_, XPathValue.Str(y)) => asString(a) == y
    case _ => false
  }

  private def applyRel(x: Double, y: Double, op: BinaryOp): Boolean = op match {
    case BinaryOp.Lt => x < y
    case BinaryOp.Le => x <= y
    case BinaryOp.Gt => x > y
    case BinaryOp.Ge => x >= y
    case _ => false
  }

  private def numberCandidates(v: XPathValue): List[Double] = v match {
    case XPathValue.NodeSet(ns) => ns.map(n => toNumberOrNaN(XPathValue.Str(stringValue(n))))
    case other => List(toNumberOrNaN(other))
  }

  private def applyArith(x: Double, y: Double, op: ArithOp): Double = op match {
    case ArithOp.Add => x + y
    case ArithOp.Sub => x - y
    case ArithOp.Mul => x * y
    case ArithOp.Div => x / y
    case ArithOp.Mod => x % y
  }

  // ==========================================================================
  // Coercions
  // ==========================================================================

  /** The XPath 1.0 `boolean()` coercion: a node-set is true iff non-empty, a string iff non-empty,
    * a number iff neither zero nor NaN.
    */
  def toBool(v: XPathValue): Boolean = v match {
    case XPathValue.Bool(b) => b
    case XPathValue.NodeSet(ns) => ns.nonEmpty
    case XPathValue.Str(s) => s.nonEmpty
    case XPathValue.Number(n) => n != 0.0 && !n.isNaN
  }

  /** The XPath 1.0 `number()` coercion: booleans map to 1/0; strings parse after trimming or NaN; a
    * node-set takes the string-value of its first node in document order (empty → NaN).
    * Unconvertible values yield NaN rather than an error, per the spec.
    */
  def toNumberOrNaN(v: XPathValue): Double = v match {
    case XPathValue.Number(n) => n
    case XPathValue.Bool(b) => if b then 1.0 else 0.0
    case XPathValue.Str(s) => numberFromString(s)
    case XPathValue.NodeSet(ns) =>
      if ns.isEmpty then Double.NaN else numberFromString(stringValue(ns.head))
  }

  private def numberFromString(s: String): Double =
    try s.trim.toDouble
    catch case _: NumberFormatException => Double.NaN

  /** The XPath 1.0 `string()` coercion: numbers via [[formatNumber]], booleans as `true`/`false`,
    * and a node-set as the string-value of its first node (`""` when empty).
    */
  def asString(v: XPathValue): String = v match {
    case XPathValue.Str(s) => s
    case XPathValue.Bool(b) => if b then "true" else "false"
    case XPathValue.Number(n) => formatNumber(n)
    case XPathValue.NodeSet(ns) => if ns.isEmpty then "" else stringValue(ns.head)
  }

  /** XPath 1.0 number-to-string: integral values print without a decimal point. */
  def formatNumber(n: Double): String =
    if n.isNaN then "NaN"
    else if n == Double.PositiveInfinity then "Infinity"
    else if n == Double.NegativeInfinity then "-Infinity"
    else if n == n.floor && n.abs < 1e15 then n.toLong.toString
    else n.toString

  // ==========================================================================
  // Functions
  // ==========================================================================

  private def evalFunction(
    name: String,
    args: List[XPathExpr],
    ctx: Context
  ): Either[XPathError, XPathValue] = {
    def evalAll(as: List[XPathExpr]): Either[XPathError, List[XPathValue]] = {
      as.foldLeft[Either[XPathError, List[XPathValue]]](Right(List.empty)) { (acc, a) =>
        for {
          tail <- acc
          v <- evalExpr(a, ctx)
        } yield tail :+ v
      }
    }

    def expectArgs(n: Int): Either[XPathError, List[XPathValue]] = {
      if args.length != n then {
        Left(XPathError.Invalid(s"$name expects $n argument(s), got ${args.length}"))
      } else {
        evalAll(args)
      }
    }

    def expectArgsAtLeast(n: Int): Either[XPathError, List[XPathValue]] = {
      if args.length < n then {
        Left(XPathError.Invalid(s"$name expects at least $n argument(s), got ${args.length}"))
      } else {
        evalAll(args)
      }
    }

    def nodesOf(v: XPathValue): Either[XPathError, List[XNode]] = v match {
      case XPathValue.NodeSet(ns) => Right(ns)
      case _ => Left(XPathError.Invalid(s"$name requires a node-set argument"))
    }

    name match {
      case "last" => {
        if args.nonEmpty then {
          Left(XPathError.Invalid("last() takes no arguments"))
        } else {
          Right(XPathValue.Number(ctx.size.toDouble))
        }
      }
      case "position" => {
        if args.nonEmpty then {
          Left(XPathError.Invalid("position() takes no arguments"))
        } else {
          Right(XPathValue.Number(ctx.position.toDouble))
        }
      }
      case "count" => {
        expectArgs(1).flatMap { vs =>
          nodesOf(vs.head).map(ns => XPathValue.Number(ns.size.toDouble))
        }
      }
      case "id" => {
        // Documented divergence: no DTD ID-typing, so no ID attributes can exist.
        expectArgs(1).map(_ => XPathValue.NodeSet(List.empty))
      }
      case "local-name" | "name" | "namespace-uri" => {
        val vs = if args.isEmpty then Right(List(XPathValue.NodeSet(List(ctx.node)))) else evalAll(args)
        vs.map { list =>
          list.head match {
            case XPathValue.NodeSet(ns) =>
              ns.minByOption(_.order) match {
                case None => XPathValue.Str("")
                case Some(n) =>
                  name match {
                    case "local-name" => XPathValue.Str(localNameOf(n))
                    case "name" => XPathValue.Str(fullNameOf(n))
                    case _ => XPathValue.Str("") // namespace-unaware (documented)
                  }
              }
            // a non-node argument has no node and therefore no name/uri (an X0-pinned decision)
            case _ => XPathValue.Str("")
          }
        }
      }
      case "string" => {
        val vs = if args.isEmpty then Right(List(XPathValue.Str(stringValue(ctx.node)))) else evalAll(args)
        vs.map(list => XPathValue.Str(asString(list.head)))
      }
      case "concat" => {
        expectArgsAtLeast(2).map(vs => XPathValue.Str(vs.map(asString).mkString))
      }
      case "starts-with" => {
        expectArgs(2).map(vs => XPathValue.Bool(asString(vs(0)).startsWith(asString(vs(1)))))
      }
      case "contains" => {
        expectArgs(2).map(vs => XPathValue.Bool(asString(vs(0)).contains(asString(vs(1)))))
      }
      case "substring-before" => {
        expectArgs(2).map { vs =>
          val s = asString(vs(0))
          val sep = asString(vs(1))
          val i = s.indexOf(sep)
          XPathValue.Str(if i < 0 then "" else s.substring(0, i))
        }
      }
      case "substring-after" => {
        expectArgs(2).map { vs =>
          val s = asString(vs(0))
          val sep = asString(vs(1))
          val i = s.indexOf(sep)
          XPathValue.Str(if i < 0 then "" else s.substring(i + sep.length))
        }
      }
      case "substring" => {
        evalAll(args).flatMap { list =>
          if list.length < 2 || list.length > 3 then {
            Left(XPathError.Invalid(s"substring expects 2 or 3 arguments, got ${list.length}"))
          } else {
            val s = asString(list(0))
            val start = xpathRound(toNumberOrNaN(list(1)))
            val lenOpt = list.drop(2).headOption.map(v => xpathRound(toNumberOrNaN(v)))
            // XPath 1.0 substring: positions are rounded; the window is [start, start+len)
            val chars = s.zipWithIndex.collect {
              case (c, i) if i + 1 >= start && lenOpt.forall(end => i + 1 < start + end) => c
            }
            Right(XPathValue.Str(chars.mkString))
          }
        }
      }
      case "string-length" => {
        val vs = if args.isEmpty then Right(List(XPathValue.Str(stringValue(ctx.node)))) else evalAll(args)
        vs.map(list => XPathValue.Number(asString(list.head).length.toDouble))
      }
      case "normalize-space" => {
        val vs = if args.isEmpty then Right(List(XPathValue.Str(stringValue(ctx.node)))) else evalAll(args)
        vs.map(list => XPathValue.Str(asString(list.head).trim.split("\\s+").filter(_.nonEmpty).mkString(" ")))
      }
      case "translate" => {
        expectArgs(3).map { vs =>
          val s = asString(vs(0))
          val from = asString(vs(1))
          val to = asString(vs(2))
          val mapped = s.map { c =>
            val i = from.indexOf(c)
            if i < 0 then c
            else if i < to.length then to.charAt(i)
            else 0.toChar
          }.filter(_ != 0.toChar)
          XPathValue.Str(mapped.mkString)
        }
      }
      case "boolean" => {
        expectArgs(1).map(vs => XPathValue.Bool(toBool(vs.head)))
      }
      case "not" => {
        expectArgs(1).map(vs => XPathValue.Bool(!toBool(vs.head)))
      }
      case "true" => {
        if args.nonEmpty then {
          Left(XPathError.Invalid("true() takes no arguments"))
        } else {
          Right(XPathValue.Bool(true))
        }
      }
      case "false" => {
        if args.nonEmpty then {
          Left(XPathError.Invalid("false() takes no arguments"))
        } else {
          Right(XPathValue.Bool(false))
        }
      }
      case "lang" => {
        expectArgs(1).map { vs =>
          val wanted = asString(vs.head).toLowerCase
          var cur: Option[XNode] = Some(ctx.node)
          var found: Option[String] = None
          while cur.isDefined && found.isEmpty do {
            cur.get match {
              case e: ElementNode =>
                e.attributes
                  .find(a => a.name.prefix.contains("xml") && a.name.localName == "lang")
                  .foreach(a => found = Some(a.value.toLowerCase))
              case _ => ()
            }
            cur = cur.get.parent
          }
          found match {
            case None => XPathValue.Bool(false)
            case Some(actual) =>
              XPathValue.Bool(actual == wanted || actual.startsWith(wanted + "-"))
          }
        }
      }
      case "number" => {
        val vs = if args.isEmpty then Right(List(XPathValue.Str(stringValue(ctx.node)))) else evalAll(args)
        vs.map(list => XPathValue.Number(toNumberOrNaN(list.head)))
      }
      case "sum" => {
        expectArgs(1).flatMap { vs =>
          nodesOf(vs.head).map(ns =>
            XPathValue.Number(ns.foldLeft(0.0)((acc, n) => acc + numberFromString(stringValue(n))))
          )
        }
      }
      case "floor" => {
        expectArgs(1).map(vs => XPathValue.Number(math.floor(toNumberOrNaN(vs.head))))
      }
      case "ceiling" => {
        expectArgs(1).map(vs => XPathValue.Number(math.ceil(toNumberOrNaN(vs.head))))
      }
      case "round" => {
        expectArgs(1).map(vs => XPathValue.Number(xpathRound(toNumberOrNaN(vs.head))))
      }
      case other => Left(XPathError.Unsupported(s"function $other()"))
    }
  }

  /** XPath 1.0 round: halfway cases round toward positive infinity (round(-0.5) = -0). */
  private def xpathRound(d: Double): Double =
    if d.isNaN then Double.NaN else math.floor(d + 0.5)

  private def localNameOf(n: XNode): String = n match {
    case e: ElementNode => e.name.localName
    case a: AttributeNode => a.name.localName
    case p: PiNode => p.target
    case _ => ""
  }

  private def fullNameOf(n: XNode): String = n match {
    case e: ElementNode =>
      e.name.prefix match {
        case Some(p) => s"$p:${e.name.localName}"
        case None => e.name.localName
      }
    case a: AttributeNode =>
      a.name.prefix match {
        case Some(p) => s"$p:${a.name.localName}"
        case None => a.name.localName
      }
    case p: PiNode => p.target
    case _ => ""
  }

  // ==========================================================================
  // Projection helpers (Spark xpath* result kinds — see the scope plan §1 table)
  // ==========================================================================

  /** `xpath` (node-set → array<string>): per-node getNodeValue semantics — element and document
    * nodes project to `None` (Spark null), everything else to its string value.
    */
  def toNodeSetValueList(v: XPathValue): Option[List[Option[String]]] = v match {
    case XPathValue.NodeSet(ns) =>
      Some(ns.map {
        case _: ElementNode | _: Document => None
        case n => Some(stringValue(n))
      })
    case _ => None
  }

  /** Truncation toward zero with Spark's NaN → 0 rule (the `xpath_int` result kind). */
  def truncateToInt(n: Double): Int = if n.isNaN then 0 else n.toInt

  /** Truncation toward zero with Spark's NaN → 0 rule (the `xpath_short` result kind). */
  def truncateToShort(n: Double): Short = if n.isNaN then 0.toShort else n.toShort

  /** Truncation toward zero with Spark's NaN → 0 rule (the `xpath_long` result kind). */
  def truncateToLong(n: Double): Long = if n.isNaN then 0L else n.toLong
}
