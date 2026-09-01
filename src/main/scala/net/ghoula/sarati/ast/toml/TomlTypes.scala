package net.ghoula.sarati.ast.toml

import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}
import scala.language.strictEquality

/** A parsed TOML value. Inline tables are the only table representation in a `TomlValue`;
  * document-level table structure uses [[TomlTable]] and flattens to a `TomlValue` via
  * [[toInlineValue]].
  */
enum TomlValue {
  case String(value: scala.Predef.String)
  case Integer(value: Long)
  case Float(value: Double)
  case Boolean(value: scala.Boolean)
  case DateTime(value: OffsetDateTime)
  case LocalDateTime(value: java.time.LocalDateTime)
  case LocalDate(value: java.time.LocalDate)
  case LocalTime(value: java.time.LocalTime)
  case Array(elements: List[TomlValue])
  case InlineTable(pairs: Map[scala.Predef.String, TomlValue])
}

object TomlValue {
  given CanEqual[TomlValue, TomlValue] = CanEqual.derived
}

/** A TOML table's key/value pairs and subtables, preserving whether it was declared as an array
  * table. [[TomlDocument]] is the root table.
  */
type TomlTable = (
  isArrayTable: scala.Boolean,
  pairs: Map[scala.Predef.String, TomlValue],
  subtables: Map[scala.Predef.String, List[TomlTable]]
)

type TomlDocument = TomlTable

/** An empty regular table. */
def emptyTable: TomlTable = (
  isArrayTable = false,
  pairs = Map.empty,
  subtables = Map.empty
)

/** A table declared with `[[name]]`. */
def arrayTable(
  pairs: Map[scala.Predef.String, TomlValue] = Map.empty,
  subtables: Map[scala.Predef.String, List[TomlTable]] = Map.empty
): TomlTable = (
  isArrayTable = true,
  pairs = pairs,
  subtables = subtables
)

/** Splits a dotted key into its path segments; empty segments are dropped. No quoting or whitespace
  * handling — `"a.b"` and `"a . b"` both split at the dot.
  */
type KeyPath = List[scala.Predef.String]

def parseKeyPath(key: scala.Predef.String): KeyPath =
  key.split('.').toList.filter(_.nonEmpty)

/** Renders a single TOML value: strings with the short escapes, floats as TOML `nan`/`inf`
  * spellings, datetimes in their `java.time` `toString` form. Keys inside inline tables quote when
  * they are not bare keys (bare: `A-Za-z0-9_-`); the same rule applies to table keys and headers in
  * [[formatToml]].
  */
def formatTomlValue(value: TomlValue): scala.Predef.String = value match {
  case TomlValue.String(s) => formatTomlString(s)
  case TomlValue.Integer(n) => n.toString
  case TomlValue.Float(d) =>
    if d.isNaN then "nan"
    else if d.isPosInfinity then "inf"
    else if d.isNegInfinity then "-inf"
    else d.toString
  case TomlValue.Boolean(b) => if b then "true" else "false"
  case TomlValue.DateTime(dt) => dt.toString
  case TomlValue.LocalDateTime(dt) => dt.toString
  case TomlValue.LocalDate(d) => d.toString
  case TomlValue.LocalTime(t) => t.toString
  case TomlValue.Array(elements) =>
    elements.map(formatTomlValue).mkString("[", ", ", "]")
  case TomlValue.InlineTable(pairs) =>
    pairs.map { case (k, v) => s"${formatTomlKey(k)} = ${formatTomlValue(v)}" }.mkString("{ ", ", ", " }")
}

/** Quotes a TOML key unless it is a bare key (nonempty, `A-Za-z0-9_-` only). */
private def formatTomlKey(key: scala.Predef.String): scala.Predef.String =
  if key.nonEmpty && key.forall(c => c.isLetterOrDigit || c == '_' || c == '-') then key
  else formatTomlString(key)

private def formatTomlString(s: scala.Predef.String): scala.Predef.String = {
  val escaped = s
    .replace("\\", "\\\\")
    .replace("\"", "\\\"")
    .replace("\n", "\\n")
    .replace("\r", "\\r")
    .replace("\t", "\\t")
  s"\"$escaped\""
}

/** Converts a parsed [[TomlTable]] (e.g. a whole `TomlDocument` from `parseToml`) into an
  * inline-table [[TomlValue]] so codec derivation can decode it: subtables become nested
  * `InlineTable`s, and array-of-tables become `Array`s of `InlineTable`s. A single regular subtable
  * stays a bare `InlineTable` (not wrapped in an array). If a pair and a subtable share a name
  * (invalid TOML), the subtable wins.
  */
def toInlineValue(table: TomlTable): TomlValue.InlineTable = {
  val subFields: Map[scala.Predef.String, TomlValue] = table.subtables.view.mapValues { tables =>
    val values = tables.map(toInlineValue)
    if tables.length == 1 && !tables.head.isArrayTable then values.head
    else TomlValue.Array(values)
  }.toMap
  TomlValue.InlineTable(table.pairs ++ subFields)
}

/** Renders a [[TomlDocument]] in table form: top-level `key = value` lines, then each subtable
  * under a `[name]` header (array tables as one `[[name]]` header per element), recursively for
  * nested subtables. Keys and header names quote when they are not bare keys. A pair and a subtable
  * sharing a name render both — producing invalid TOML; parsers cannot produce such tables (a
  * header name displaces a same-named key).
  */
def formatToml(doc: TomlDocument): scala.Predef.String = {
  def render(table: TomlTable, path: List[String]): List[String] = {
    val pairLines = table.pairs.toList.map { case (k, v) => s"${formatTomlKey(k)} = ${formatTomlValue(v)}" }
    val subtableLines = table.subtables.toList.flatMap { case (name, tables) =>
      val headerPath = path :+ formatTomlKey(name)
      val header = headerPath.mkString(".")
      tables.flatMap { t =>
        val headerLine = t.isArrayTable match {
          case true => s"[[$header]]"
          case false => s"[$header]"
        }
        "" :: headerLine :: render(t, headerPath)
      }
    }
    pairLines ++ subtableLines
  }

  render(doc, List.empty).mkString("\n") match {
    case "" => ""
    case text => text.stripPrefix("\n")
  }
}
