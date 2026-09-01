package net.ghoula.sarati.ast.xml

import scala.language.strictEquality

/** An XML name: optional prefix plus local name. `qname("ns:b")` splits at the colon; names with
  * more than one colon keep the full string as the local name with no prefix.
  */
type QName = (prefix: Option[String], localName: String)

given CanEqual[QName, QName] = CanEqual.derived

def qname(name: String): QName =
  name.split(':').toList match {
    case List(local) => (prefix = None, localName = local)
    case List(pre, local) => (prefix = Some(pre), localName = local)
    case _ => (prefix = None, localName = name)
  }

def qnameWith(prefix: String, localName: String): QName =
  (prefix = Some(prefix), localName = localName)

/** An attribute: name and value. Values are entity-expanded text; attribute ordering is the
  * document's.
  */
type XmlAttribute = (name: QName, value: String)

/** A `xmlns`-style declaration carried as data; namespace URIs are not resolved anywhere in the AST
  * — name matching elsewhere is prefix-literal.
  */
type NamespaceDecl = (prefix: Option[String], uri: String)

/** An XML document's content: elements with attributes and children, text (whitespace preserved or
  * trimmed per the parser config), CDATA (distinct from text until an XPath evaluation folds it
  * in), comments, and processing instructions.
  */
enum XmlNode {
  case Element(
    name: QName,
    attributes: List[XmlAttribute],
    children: List[XmlNode]
  )
  case Text(content: String)
  case CData(content: String)
  case Comment(content: String)
  case ProcessingInstruction(target: String, content: String)
}

object XmlNode {
  given CanEqual[XmlNode, XmlNode] = CanEqual.derived
}

/** A document: XML declaration fields and the root node. */
type XmlDocument = (
  version: String,
  encoding: Option[String],
  standalone: Option[Boolean],
  root: XmlNode
)

val defaultXmlVersion: String = "1.0"
val defaultXmlEncoding: Option[String] = Some("UTF-8")

/** Parser behavior switches: whether inter-element whitespace becomes text nodes, and whether
  * comments, processing instructions, and entity references survive into the AST.
  */
type XmlConfig = (
  preserveWhitespace: Boolean,
  parseComments: Boolean,
  parseProcessingInstructions: Boolean,
  expandEntities: Boolean
)

/** Trims whitespace text nodes, keeps comments, PIs, and entity expansion. */
val defaultXmlConfig: XmlConfig = (
  preserveWhitespace = false,
  parseComments = true,
  parseProcessingInstructions = true,
  expandEntities = true
)

/** Preserves whitespace text nodes and disables comments, PIs, and entity expansion. */
val strictXmlConfig: XmlConfig = (
  preserveWhitespace = true,
  parseComments = false,
  parseProcessingInstructions = false,
  expandEntities = false
)

/** Whitespace preserved while comments, processing instructions, and entity expansion stay enabled
  * — the combination XPath evaluation needs, since string-value and `text()` semantics depend on
  * whitespace text nodes and node tests must see comments and PIs.
  */
val xpathXmlConfig: XmlConfig = (
  preserveWhitespace = true,
  parseComments = true,
  parseProcessingInstructions = true,
  expandEntities = true
)

/** The five predefined entities. */
val xmlEntities: Map[String, String] = Map(
  "lt" -> "<",
  "gt" -> ">",
  "amp" -> "&",
  "quot" -> "\"",
  "apos" -> "'"
)

/** Renders one node. An element with a single text child prints inline; otherwise children print
  * one per line at `depth + 1`. Text and attribute values escape `&`, `<`, `>` (and quotes in
  * attributes). The `standalone` declaration field is not rendered.
  */
def formatXml(node: XmlNode, indent: Int = 2, depth: Int = 0): String = {
  val pad = " " * (indent * depth)
  node match {
    case XmlNode.Element(name, attrs, children) =>
      val tag = formatQName(name)
      val attrsStr = attrs match {
        case Nil => ""
        case as => " " + as.map { a => s"${formatQName(a.name)}=\"${escapeXmlAttr(a.value)}\"" }.mkString(" ")
      }
      children match {
        case Nil =>
          s"$pad<$tag$attrsStr/>"
        case List(XmlNode.Text(content)) =>
          s"$pad<$tag$attrsStr>${escapeXmlText(content)}</$tag>"
        case _ =>
          val inner = children.map(c => formatXml(c, indent, depth + 1)).mkString("\n")
          s"$pad<$tag$attrsStr>\n$inner\n$pad</$tag>"
      }
    case XmlNode.Text(content) =>
      s"$pad${escapeXmlText(content)}"
    case XmlNode.CData(content) =>
      s"$pad<![CDATA[$content]]>"
    case XmlNode.Comment(content) =>
      s"$pad<!--$content-->"
    case XmlNode.ProcessingInstruction(target, content) =>
      s"$pad<?$target $content?>"
  }
}

/** Renders the declaration (`version` + `encoding` when present) and the root element. */
def formatXmlDocument(doc: XmlDocument, indent: Int = 2): String = {
  val encoding = doc.encoding.map(e => s""" encoding="$e"""").getOrElse("")
  val decl = s"""<?xml version="${doc.version}"$encoding?>"""
  s"$decl\n${formatXml(doc.root, indent, 0)}"
}

private def formatQName(qn: QName): String =
  qn.prefix match {
    case Some(prefix) => s"$prefix:${qn.localName}"
    case None => qn.localName
  }

private def escapeXmlText(text: String): String =
  text
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")

private def escapeXmlAttr(value: String): String =
  value
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&apos;")
