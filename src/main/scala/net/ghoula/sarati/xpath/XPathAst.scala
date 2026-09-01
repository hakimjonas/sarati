package net.ghoula.sarati.xpath

/** The XPath 1.0 axis set.
  *
  * Reverse axes ([[Parent]], [[Ancestor]], [[PrecedingSibling]], [[Preceding]], [[AncestorOrSelf]])
  * number their nodes in reverse document order for predicate proximity positions; all other axes
  * are forward.
  */
enum Axis {
  case Child
  case Descendant
  case Parent
  case Ancestor
  case FollowingSibling
  case PrecedingSibling
  case Following
  case Preceding
  case Attribute
  case Namespace
  case Self
  case DescendantOrSelf
  case AncestorOrSelf
}

object Axis {
  // Term patterns (case Axis.Parent | ...) compile to equality checks — under
  // -language:strictEquality they need this given.
  given CanEqual[Axis, Axis] = CanEqual.derived

  /** True for the axes whose proximity positions count in reverse document order. */
  def isReverse(axis: Axis): Boolean = axis match {
    case Axis.Parent | Axis.Ancestor | Axis.PrecedingSibling | Axis.Preceding | Axis.AncestorOrSelf =>
      true
    case _ => false
  }
}

/** XPath 1.0 name tests, distinguishing the three wildcard shapes. */
enum NameTest {
  case Any // *
  case AnyLocalInNamespace(prefix: String) // ns:*
  case AnyNamespace(local: String) // *:b
  case Named(prefix: Option[String], local: String) // qname — prefix-literal matching
}

/** XPath 1.0 node tests. */
enum NodeTest {
  case Name(test: NameTest)
  case Node
  case Text
  case Comment
  case ProcessingInstruction(name: Option[String])
}

object NodeTest {
  given CanEqual[NodeTest, NodeTest] = CanEqual.derived
}

object NameTest {
  given CanEqual[NameTest, NameTest] = CanEqual.derived
}

/** One location-path step: an axis, a node test, and zero or more predicates. Predicates filter
  * left to right over the survivors of the previous one; each sees proximity positions in the
  * axis's own order ([[Axis.isReverse]]), and a numeric predicate tests that position.
  */
case class Step(axis: Axis, test: NodeTest, predicates: List[XPathExpr])

/** XPath 1.0 comparison operators. Comparisons involving node-sets are existential: the operator
  * holds if any pairing of nodes with the other side satisfies it.
  */
enum BinaryOp {
  case Eq, Ne, Lt, Le, Gt, Ge
}

object BinaryOp {
  given CanEqual[BinaryOp, BinaryOp] = CanEqual.derived
}

/** XPath 1.0 arithmetic operators (`Div` is the `/` operator, `Mod` the `mod` keyword). Operands
  * coerce via [[XPathEval.toNumberOrNaN]] and results follow IEEE 754 — NaN propagates, division is
  * never integer.
  */
enum ArithOp {
  case Add, Sub, Mul, Div, Mod
}

object ArithOp {
  given CanEqual[ArithOp, ArithOp] = CanEqual.derived
}

/** The XPath 1.0 expression AST, evaluated by [[XPathEval]]. Text parsers build it.
  */
enum XPathExpr {
  case Or(left: XPathExpr, right: XPathExpr)
  case And(left: XPathExpr, right: XPathExpr)
  case Comparison(left: XPathExpr, right: XPathExpr, op: BinaryOp)
  case Arithmetic(left: XPathExpr, right: XPathExpr, op: ArithOp)
  case Negation(inner: XPathExpr)
  case Union(left: XPathExpr, right: XPathExpr)

  /** A location path. `isAbsolute` marks `/`-rooted paths; the single-step case `/` alone has an
    * empty `steps` list and selects the document node.
    */
  case Path(isAbsolute: Boolean, steps: List[Step])

  /** A filtered primary expression (`$var[1]`, `(expr)//x`) with its filter predicates, followed by
    * location steps. `predicates` filter the primary's node-set with proximity positions running
    * over the node-set in document order (XPath 1.0 §2.4/§3.3, matching Spark's JDK engine);
    * `steps` then evaluate against the survivors.
    */
  case Filter(primary: XPathExpr, predicates: List[XPathExpr], steps: List[Step])
  case Literal(value: String)
  case Number(value: Double)
  case VariableRef(name: String)
  case FunctionCall(name: String, args: List[XPathExpr])
}
