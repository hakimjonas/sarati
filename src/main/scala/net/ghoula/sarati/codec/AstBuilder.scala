package net.ghoula.sarati.codec

import net.ghoula.sarati.ast.json.JsonValue
import net.ghoula.sarati.ast.toml.TomlValue
import net.ghoula.sarati.ast.xml.{XmlNode, qname}
import net.ghoula.sarati.ast.yaml.YamlValue

/** Construction of AST nodes from primitive values, as used by `Encoder.derived`.
  *
  * Instances for the four AST types are provided; derivation fails at compile time without one.
  * Behavioral notes per AST: TOML has no null, so `fromNull` yields an empty `String`; XML has no
  * primitives, so scalars become `Text` nodes (numbers via `Long` when whole, decimal otherwise),
  * objects become an `<object>` element with one child element per field, and arrays become an
  * `<array>` element.
  */
trait AstBuilder[AST] {
  def createObject(fields: Map[String, AST]): AST
  def createArray(elements: List[AST]): AST
  def fromString(s: String): AST
  def fromNumber(n: Double): AST
  def fromBoolean(b: Boolean): AST
  def fromNull: AST
}

object AstBuilder {

  given AstBuilder[JsonValue] with {
    def createObject(fields: Map[String, JsonValue]): JsonValue = JsonValue.Object(fields)
    def createArray(elements: List[JsonValue]): JsonValue = JsonValue.Array(elements)
    def fromString(s: String): JsonValue = JsonValue.Str(s)
    def fromNumber(n: Double): JsonValue = JsonValue.Number(n)
    def fromBoolean(b: Boolean): JsonValue = JsonValue.Bool(b)
    def fromNull: JsonValue = JsonValue.Null
  }

  given AstBuilder[YamlValue] with {
    def createObject(fields: Map[String, YamlValue]): YamlValue = YamlValue.Mapping(fields)
    def createArray(elements: List[YamlValue]): YamlValue = YamlValue.Sequence(elements)
    def fromString(s: String): YamlValue = YamlValue.String(s)
    def fromNumber(n: Double): YamlValue = YamlValue.Float(n)
    def fromBoolean(b: Boolean): YamlValue = YamlValue.Boolean(b)
    def fromNull: YamlValue = YamlValue.Null
  }

  given AstBuilder[TomlValue] with {
    def createObject(fields: Map[String, TomlValue]): TomlValue = TomlValue.InlineTable(fields)
    def createArray(elements: List[TomlValue]): TomlValue = TomlValue.Array(elements)
    def fromString(s: String): TomlValue = TomlValue.String(s)
    def fromNumber(n: Double): TomlValue = TomlValue.Float(n)
    def fromBoolean(b: Boolean): TomlValue = TomlValue.Boolean(b)
    def fromNull: TomlValue = TomlValue.String("")
  }

  given AstBuilder[XmlNode] with {
    def createObject(fields: Map[String, XmlNode]): XmlNode =
      XmlNode.Element(
        qname("object"),
        List.empty,
        fields.map { case (k, v) =>
          XmlNode.Element(qname(k), List.empty, List(v))
        }.toList
      )
    def createArray(elements: List[XmlNode]): XmlNode =
      XmlNode.Element(qname("array"), List.empty, elements)
    def fromString(s: String): XmlNode = XmlNode.Text(s)
    def fromNumber(n: Double): XmlNode =
      XmlNode.Text(if n.isWhole then n.toLong.toString else n.toString)
    def fromBoolean(b: Boolean): XmlNode = XmlNode.Text(b.toString)
    def fromNull: XmlNode = XmlNode.Text("")
  }
}
