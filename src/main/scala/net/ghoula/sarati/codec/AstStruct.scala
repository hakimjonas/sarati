package net.ghoula.sarati.codec

import net.ghoula.sarati.ast.json.JsonValue
import net.ghoula.sarati.ast.toml.TomlValue
import net.ghoula.sarati.ast.xml.XmlNode
import net.ghoula.sarati.ast.yaml.YamlValue

/** Field access into an AST's "struct" shape, as used by `Decoder.derived`.
  *
  * The struct shape per AST: JSON `Object`, TOML `InlineTable`, YAML `Mapping`, XML `Element`.
  * Instances for those four types are provided; derivation fails at compile time without one.
  */
trait AstStruct[AST] {
  def isStruct(ast: AST): Boolean

  /** The named field's value. XML reads the **first** matching element child (by `localName`,
    * ignoring the prefix); repeated sibling elements are not modeled as a collection here — use a
    * `List` decoder over the parent's children for that.
    */
  def getField(ast: AST, fieldName: String): Option[AST]

  /** The struct type's display name, for `TypeMismatch` errors. */
  def expectedName: String

  /** The actual node's display name, for `TypeMismatch` errors. */
  def actualName(ast: AST): String
}

object AstStruct {

  /** JSON: fields come from `Object`'s map; duplicate keys cannot occur in a `Map`. */
  given AstStruct[JsonValue] with {
    def isStruct(ast: JsonValue): Boolean = ast match {
      case _: JsonValue.Object => true
      case _ => false
    }
    def getField(ast: JsonValue, name: String): Option[JsonValue] = ast match {
      case JsonValue.Object(fields) => fields.get(name)
      case _ => None
    }
    def expectedName: String = "Object"
    def actualName(ast: JsonValue): String = ast match {
      case JsonValue.Null => "Null"
      case JsonValue.Bool(_) => "Boolean"
      case JsonValue.Number(_, _) => "Number"
      case JsonValue.Str(_) => "String"
      case JsonValue.Array(_) => "Array"
      case JsonValue.Object(_) => "Object"
    }
  }

  /** TOML: fields come from `InlineTable` pairs only. Table-structure documents decode through
    * [[net.ghoula.sarati.ast.toml.toInlineValue]] first, which flattens subtables and
    * arrays-of-tables into nested inline tables.
    */
  given AstStruct[TomlValue] with {
    def isStruct(ast: TomlValue): Boolean = ast match {
      case _: TomlValue.InlineTable => true
      case _ => false
    }
    def getField(ast: TomlValue, name: String): Option[TomlValue] = ast match {
      case TomlValue.InlineTable(fields) => fields.get(name)
      case _ => None
    }
    def expectedName: String = "InlineTable"
    def actualName(ast: TomlValue): String = ast match {
      case TomlValue.String(_) => "String"
      case TomlValue.Integer(_) => "Integer"
      case TomlValue.Float(_) => "Float"
      case TomlValue.Boolean(_) => "Boolean"
      case TomlValue.DateTime(_) => "DateTime"
      case TomlValue.LocalDateTime(_) => "LocalDateTime"
      case TomlValue.LocalDate(_) => "LocalDate"
      case TomlValue.LocalTime(_) => "LocalTime"
      case TomlValue.Array(_) => "Array"
      case TomlValue.InlineTable(_) => "InlineTable"
    }
  }

  given AstStruct[YamlValue] with {
    def isStruct(ast: YamlValue): Boolean = ast match {
      case _: YamlValue.Mapping => true
      case _ => false
    }
    def getField(ast: YamlValue, name: String): Option[YamlValue] = ast match {
      case YamlValue.Mapping(pairs) => pairs.get(name)
      case _ => None
    }
    def expectedName: String = "Mapping"
    def actualName(ast: YamlValue): String = ast match {
      case YamlValue.Null => "Null"
      case YamlValue.Boolean(_) => "Boolean"
      case YamlValue.Integer(_) => "Integer"
      case YamlValue.Float(_) => "Float"
      case YamlValue.String(_) => "String"
      case YamlValue.Sequence(_) => "Sequence"
      case YamlValue.Mapping(_) => "Mapping"
    }
  }

  /** XML: fields are child elements matched by `localName` — the first match wins; attributes are
    * not fields.
    */
  given AstStruct[XmlNode] with {
    def isStruct(ast: XmlNode): Boolean = ast match {
      case _: XmlNode.Element => true
      case _ => false
    }
    def getField(ast: XmlNode, name: String): Option[XmlNode] = ast match {
      case XmlNode.Element(_, _, children) =>
        children.collectFirst { case e: XmlNode.Element if e.name.localName == name => e }
      case _ => None
    }
    def expectedName: String = "Element"
    def actualName(ast: XmlNode): String = ast match {
      case XmlNode.Element(name, _, _) => s"Element(${name.localName})"
      case XmlNode.Text(_) => "Text"
      case XmlNode.CData(_) => "CData"
      case XmlNode.Comment(_) => "Comment"
      case XmlNode.ProcessingInstruction(_, _) => "ProcessingInstruction"
    }
  }
}
