package net.ghoula.sarati.ast.json

import scala.language.strictEquality

/** A parsed JSON value. Numbers are `Double` — JSON integers outside the exact-`Double` range
  * (2^53) lose precision once parsed; there is no integer variant.
  */
enum JsonValue {
  case Null
  case Bool(value: Boolean)

  /** A JSON number: `value` is the parsed double, `raw` the token exactly as it appeared in the
    * text (`Some("1.0")`, `Some("1e10")`, `Some("1E+5")` are distinct spellings of the same double)
    * — `None` when constructed programmatically, where no token exists. Equality includes `raw`;
    * consumers needing the original spelling fall back to the canonical form when `raw` is empty.
    */
  case Number(value: Double, raw: Option[String] = None)
  case Str(value: String)
  case Array(elements: List[JsonValue])
  case Object(fields: Map[String, JsonValue])
}

object JsonValue {
  given CanEqual[JsonValue, JsonValue] = CanEqual.derived
}

/** Formatter configuration: `indent` spaces per level, `newlines` between elements, and optional
  * key sorting ([[sortKeys]]). Object key order is the map's iteration order unless sorted.
  */
type JsonFormatConfig = (
  indent: Int,
  newlines: Boolean,
  sortKeys: Boolean
)

/** Single-line output, keys in iteration order. */
val compactFormat: JsonFormatConfig = (
  indent = 0,
  newlines = false,
  sortKeys = false
)

/** Two-space indented, one element per line, keys in iteration order. */
val prettyFormat: JsonFormatConfig = (
  indent = 2,
  newlines = true,
  sortKeys = false
)

/** Renders `value` as JSON text. Strings escape `\`, `"`, the C0 control characters, and the
  * standard short escapes; whole numbers within `Long` range print without a decimal point (`1` not
  * `1.0`), larger whole numbers print in exponent form (`1.0E20`), and non-finite doubles (`NaN`,
  * the infinities — unrepresentable in JSON) print as `null`. The formatter is canonical: a
  * [[JsonValue.Number]]'s preserved `raw` token is not consulted.
  */
def formatJson(value: JsonValue, config: JsonFormatConfig = compactFormat): String =
  formatJsonValue(value, 0, config)

private def formatJsonValue(value: JsonValue, depth: Int, config: JsonFormatConfig): String =
  value match {
    case JsonValue.Null => "null"
    case JsonValue.Bool(b) => b.toString
    case JsonValue.Number(n, _) =>
      n match {
        case _ if n.isNaN || n.isInfinite => "null"
        case _ if n.isWhole && n.abs <= Long.MaxValue.toDouble => n.toLong.toString
        case _ => n.toString
      }
    case JsonValue.Str(s) => formatJsonString(s)
    case JsonValue.Array(elements) => formatJsonArray(elements, depth, config)
    case JsonValue.Object(fields) => formatJsonObject(fields, depth, config)
  }

private def formatJsonString(s: String): String = {
  val escaped = s.flatMap {
    case '\\' => "\\\\"
    case '"' => "\\\""
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case c if c < '\u0020' => f"\\u${c.toInt}%04x"
    case c => c.toString
  }
  s"\"$escaped\""
}

private def formatJsonArray(
  elements: List[JsonValue],
  depth: Int,
  config: JsonFormatConfig
): String =
  elements match {
    case Nil => "[]"
    case _ =>
      val inner = elements.map(e => formatJsonValue(e, depth + 1, config))
      config.newlines match {
        case true =>
          val pad = " " * (config.indent * (depth + 1))
          val closePad = " " * (config.indent * depth)
          inner.map(s => s"$pad$s").mkString("[\n", ",\n", s"\n$closePad]")
        case false =>
          inner.mkString("[", ",", "]")
      }
  }

private def formatJsonObject(
  fields: Map[String, JsonValue],
  depth: Int,
  config: JsonFormatConfig
): String =
  fields.toList match {
    case Nil => "{}"
    case pairs =>
      val sorted = if config.sortKeys then pairs.sortBy(_._1) else pairs
      val inner = sorted.map { case (k, v) =>
        s"${formatJsonString(k)}:${formatJsonValue(v, depth + 1, config)}"
      }
      config.newlines match {
        case true =>
          val pad = " " * (config.indent * (depth + 1))
          val closePad = " " * (config.indent * depth)
          inner.map(s => s"$pad$s").mkString("{\n", ",\n", s"\n$closePad}")
        case false =>
          inner.mkString("{", ",", "}")
      }
  }
