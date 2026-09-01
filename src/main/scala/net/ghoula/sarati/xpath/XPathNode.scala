package net.ghoula.sarati.xpath

import net.ghoula.sarati.ast.xml.*

/** The XPath node model over Sarati's [[XmlNode]] AST: every node carries its position in document
  * order and its owning [[Document]], and [[parent]] navigates through the document. CDATA sections
  * fold into text nodes ([[TextNode.cdata]]): they contribute to string values and `node()` like
  * ordinary text, while the `text()` node test excludes them — matching the JDK engine Spark
  * evaluates `xpath*` with.
  */
sealed trait XNode {
  def doc: Document
  def order: Int
  def children: List[XNode]

  /** The innermost element whose `[start, end)` interval contains this node's order; the document
    * node itself for the root element; `None` for the document.
    */
  def parent: Option[XNode] = doc.parentOf(order)
}

/** The document node (`/`). Document order 0 is the document itself; the root element and
  * everything below it occupy orders 1..n. The document carries the wrapped document's node index:
  * [[allNodes]] in document order, [[root]], and [[parentOf]] for parent and ancestor navigation.
  * Attributes sit in document order directly after their element.
  */
final class Document private[xpath] () extends XNode {
  private[xpath] var nodesSlot: Vector[XNode] = Vector.empty
  private[xpath] var rootSlot: Option[ElementNode] = None
  // parallel vectors of element interval starts/ends and the elements themselves, sorted by start
  private[xpath] var elemStarts: Vector[Int] = Vector.empty
  private[xpath] var elemEnds: Vector[Int] = Vector.empty
  private[xpath] var elemNodes: Vector[ElementNode] = Vector.empty

  def doc: Document = this
  def order: Int = 0
  def root: ElementNode = rootSlot.get
  def children: List[XNode] = rootSlot.toList
  def allNodes: Vector[XNode] = nodesSlot

  /** The parent of the node with the given document order: the innermost element interval
    * containing it, or the document for the root element.
    */
  def parentOf(order: Int): Option[XNode] =
    if order <= 0 then None
    else {
      // The containing element is the one with the LARGEST start < order whose interval still
      // reaches the order. Intervals nest, so candidates shrink monotonically walking left from
      // the rightmost start < order (a preceding sibling that already ended fails the end check).
      var lo = 0
      var hi = elemStarts.length
      while lo < hi do {
        val mid = (lo + hi) / 2
        if elemStarts(mid) < order then lo = mid + 1 else hi = mid
      }
      var idx = lo - 1
      while idx >= 0 && order >= elemEnds(idx) do idx -= 1
      if idx >= 0 then Some(elemNodes(idx))
      else Some(this) // no containing element: the node sits directly under the document
    }
}

/** An element node. `attributes` are its attribute nodes in document order; per XPath 1.0
  * attributes are properties of their element, not child nodes, so they never appear in `children`
  * or on the child axis. `children` lists the non-attribute content in document order, and the
  * subtree spans the `[order, endOrder)` interval — attributes and descendants both fall inside.
  */
final class ElementNode(
  val name: QName,
  val attributes: List[AttributeNode],
  val children: List[XNode],
  val doc: Document,
  val order: Int,
  /** Exclusive end of this element's subtree in document order (its interval is `[order, endOrder)`
    * — attributes and all descendants fall inside).
    */
  val endOrder: Int
) extends XNode

/** An attribute node. Childless — per XPath 1.0 an attribute is a property of its element, not a
  * child node — and its document order sits directly after the owning element's order, before the
  * element's other children.
  */
final class AttributeNode(
  val name: QName,
  val value: String,
  val doc: Document,
  val order: Int
) extends XNode {
  def children: List[XNode] = List.empty
}

/** A text node. `cdata` marks a node folded from a CDATA section: it contributes to string values,
  * `node()`, and node-value projections exactly like ordinary text, but the `text()` node test
  * excludes it — the behavior of the JDK engine Spark evaluates `xpath*` with, whose DOM keeps
  * CDATA a distinct node kind.
  */
final class TextNode(val text: String, val doc: Document, val order: Int, val cdata: Boolean = false) extends XNode {
  def children: List[XNode] = List.empty
}

/** A comment node; `text` is the comment content without the `<!--` / `-->` delimiters. */
final class CommentNode(val text: String, val doc: Document, val order: Int) extends XNode {
  def children: List[XNode] = List.empty
}

/** A processing-instruction node: `target` is the PI name (`name()` and the
  * `processing-instruction()` node test match on it), `content` the data after the target.
  */
final class PiNode(val target: String, val content: String, val doc: Document, val order: Int) extends XNode {
  def children: List[XNode] = List.empty
}

/** The XPath 1.0 object model: one of the four value types. Node-sets are kept in document order;
  * numbers are IEEE 754 doubles, NaN and ±Infinity included.
  */
enum XPathValue {
  case NodeSet(nodes: List[XNode]) // kept in document order
  case Bool(value: Boolean)
  case Number(value: Double)
  case Str(value: String)
}

/** Evaluation errors: [[Unsupported]] for constructs outside this evaluator's scope (variable
  * references, functions outside the core library), [[Invalid]] for type and arity errors on
  * implemented features.
  */
enum XPathError {
  case Unsupported(feature: String)
  case Invalid(message: String)
}

/** Evaluation context: the context node plus its proximity position and context size — the values
  * `position()` and `last()` see. Reverse axes ([[Axis.isReverse]]) number proximity positions in
  * reverse document order, so the same node can carry different positions on different axes.
  */
final case class Context(node: XNode, position: Int, size: Int)

/** Flattened pre-order entry. `order` is the entry's document order; `parentOrder` is the parent
  * element's order (0 = the document).
  */
private[xpath] final case class Entry(order: Int, raw: XmlNode, parentOrder: Int)

private[xpath] final case class AttrEntry(order: Int, name: QName, value: String, parentOrder: Int)

/** Wraps a parsed [[XmlDocument]] into the XPath node model so [[XPathEval]] can evaluate over it:
  * every element, attribute, text, comment, and processing-instruction node receives its
  * document-order index, parent/ancestor navigation works through [[Document.parentOf]], and the
  * build itself is iterative (an explicit worklist and interval scan, no recursion), so deeply
  * nested documents do not overflow the stack.
  */
def wrapDocument(doc: XmlDocument): Document = {
  // pass 1: pre-order flatten; entries are 1-indexed by document order (0 = the document)
  val entries = scala.collection.mutable.ArrayBuffer.empty[Either[AttrEntry, Entry]]
  val parentOrders = scala.collection.mutable.ArrayBuffer.empty[Int] // per entry, by order - 1
  var counter = 1
  val stack = scala.collection.mutable.ListBuffer.empty[(XmlNode, Int)]
  stack.prepend((doc.root, 0))
  while stack.nonEmpty do {
    val (raw, parentOrder) = stack.remove(0)
    val order = counter
    counter += 1
    parentOrders += parentOrder
    entries += Right(Entry(order, raw, parentOrder))
    raw match {
      case e: XmlNode.Element =>
        e.attributes.reverse.foreach { a =>
          counter += 1
          parentOrders += order
          entries += Left(AttrEntry(order, a.name, a.value, order))
        }
        e.children.reverse.foreach(c => stack.prepend((c, order)))
      case _ => ()
    }
  }
  val entryCount = counter - 1

  // depths: parents precede children in pre-order, so one forward pass suffices
  val depths = scala.collection.mutable.ArrayBuffer.fill[Int](entryCount + 1)(-1)
  for o <- 1 to entryCount do {
    val p = parentOrders(o - 1)
    depths(o) = if p == 0 then 0 else depths(p) + 1
  }

  // interval ends: forward scan with a (depth, order) stack — an element's interval ends at the
  // order of the first LATER entry at the same or shallower depth (a sibling or an ancestor's
  // continuation), so open deeper entries pop when such an entry arrives
  val endOrders = scala.collection.mutable.ArrayBuffer.fill[Int](entryCount + 1)(entryCount + 1)
  val scan = scala.collection.mutable.ListBuffer.empty[(Int, Int)] // (depth, order), deepest last
  for o <- Range(1, entryCount + 1) do {
    val d = depths(o)
    while scan.nonEmpty && scan.last._1 >= d do {
      val poppedOrder = scan.last._2
      scan.remove(scan.length - 1)
      endOrders(poppedOrder) = o
    }
    scan += ((d, o))
  }

  def nodeEntryByParent: Map[Int, List[Int]] = {
    val grouped = entries.zipWithIndex.collect { case (Right(e), i) => (e.parentOrder, i + 1) }
    grouped.groupMap(_._1)(_._2).view.mapValues(_.toList).toMap
  }

  // pass 3: bottom-up build
  val docNode = new Document
  val built = scala.collection.mutable.HashMap.empty[Int, XNode]
  val attrEntriesByParent =
    entries.zipWithIndex.collect { case (Left(a), i) => (a.parentOrder, i) }.groupMap(_._1)(_._2)
  for o <- Range(entryCount, 0, -1) do {
    val xn: XNode = entries(o - 1) match {
      case Left(a: AttrEntry) => AttributeNode(a.name, a.value, docNode, a.order)
      case Right(Entry(order, xml, _)) =>
        xml match {
          case e: XmlNode.Element =>
            val attrs = attrEntriesByParent
              .getOrElse(order, List.empty[Int])
              .toList
              .map { i =>
                entries(i) match {
                  case Left(a: AttrEntry) => AttributeNode(a.name, a.value, docNode, a.order)
                  case _ => throw new IllegalStateException("attr slot mismatch")
                }
              }
            val children = nodeEntryByParent
              .getOrElse(order, List.empty[Int])
              .map(childOrder => built(childOrder))
            ElementNode(e.name, attrs, children, docNode, order, endOrders(order))
          case XmlNode.Text(t) => TextNode(t, docNode, order)
          case XmlNode.CData(t) => TextNode(t, docNode, order, cdata = true)
          case XmlNode.Comment(t) => CommentNode(t, docNode, order)
          case XmlNode.ProcessingInstruction(target, content) =>
            PiNode(target, content, docNode, order)
        }
    }
    built(o) = xn
  }

  docNode.nodesSlot = (1 to entryCount).toList.map(o => built(o)).toVector
  // the map is keyed by construction: order 1 is always the root element
  docNode.rootSlot = Some(built(1).asInstanceOf[ElementNode]) // scalafix:ok DisableSyntax.asInstanceOf
  val elemOrders = (1 to entryCount).toList.filter { o =>
    entries(o - 1) match {
      case Right(Entry(_, _: XmlNode.Element, _)) => true
      case _ => false
    }
  }
  docNode.elemStarts = elemOrders.toVector
  docNode.elemEnds = elemOrders.map(o => endOrders(o)).toVector
  docNode.elemNodes =
    elemOrders.map(o => built(o).asInstanceOf[ElementNode]).toVector // scalafix:ok DisableSyntax.asInstanceOf
  docNode
}
