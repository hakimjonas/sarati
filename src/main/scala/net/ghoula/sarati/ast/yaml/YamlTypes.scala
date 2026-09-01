package net.ghoula.sarati.ast.yaml

import scala.language.strictEquality

/** A parsed YAML value. Scalars carry their inferred types: `Integer`/`Float`/`Boolean`/`Null`
  * reflect what the text layer recognized, and quoted or unresolvable scalars become `String`.
  */
enum YamlValue {
  case Null
  case Boolean(value: scala.Boolean)
  case Integer(value: Long)
  case Float(value: Double)
  case String(value: scala.Predef.String)
  case Sequence(elements: List[YamlValue])
  case Mapping(pairs: Map[scala.Predef.String, YamlValue])
}

object YamlValue {
  given CanEqual[YamlValue, YamlValue] = CanEqual.derived
}

/** A document's root value plus any verbatim directive lines (`%YAML`, `%TAG`). */
type YamlDocument = (
  root: YamlValue,
  directives: List[scala.Predef.String]
)

/** Renders `value` as YAML text at the given indent width. Strings and keys quote when required —
  * colliding with `true`/`false`/`null`/`~`/empty, or containing `:`, `#`, quotes, backslashes,
  * braces, brackets, commas, or newlines (keys additionally when they contain spaces) — with
  * `\`-escapes inside double quotes. Floats print as `.nan`/`.inf`/`-.inf` spellings where
  * applicable; block sequences and mappings nest under their key.
  */
def formatYaml(value: YamlValue, indent: Int = 2, depth: Int = 0): scala.Predef.String = {
  val pad = " " * (indent * depth)
  value match {
    case YamlValue.Null => s"${pad}null"
    case YamlValue.Boolean(b) => s"$pad$b"
    case YamlValue.Integer(n) => s"$pad$n"
    case YamlValue.Float(d) =>
      val repr = d match {
        case _ if d.isNaN => ".nan"
        case _ if d.isPosInfinity => ".inf"
        case _ if d.isNegInfinity => "-.inf"
        case _ => d.toString
      }
      s"$pad$repr"
    case YamlValue.String(s) => s"$pad${quoteYamlString(s)}"
    case YamlValue.Sequence(elements) =>
      elements match {
        case Nil => s"$pad[]"
        case _ =>
          elements.map { e =>
            val formatted = formatYaml(e, indent, depth + 1).stripLeading().nn
            s"$pad- $formatted"
          }.mkString("\n")
      }
    case YamlValue.Mapping(pairs) =>
      pairs.toList match {
        case Nil => s"$pad{}"
        case entries =>
          entries.map { case (k, v) =>
            v match {
              case _: YamlValue.Mapping | _: YamlValue.Sequence =>
                s"$pad${quoteYamlKey(k)}:\n${formatYaml(v, indent, depth + 1)}"
              case _ =>
                val formatted = formatYaml(v, indent, 0).stripLeading().nn
                s"$pad${quoteYamlKey(k)}: $formatted"
            }
          }.mkString("\n")
      }
  }
}

/** Renders directives (if any), the `---` document start marker, and the root value. */
def formatYamlDocument(doc: YamlDocument): scala.Predef.String = {
  val directives = doc.directives match {
    case Nil => ""
    case ds => ds.mkString("", "\n", "\n")
  }
  s"$directives---\n${formatYaml(doc.root)}"
}

private def quoteYamlString(s: scala.Predef.String): scala.Predef.String =
  s match {
    case "true" | "false" | "null" | "~" | "" => s"\"$s\""
    case _ if needsYamlQuoting(s) => s"\"${escapeYaml(s)}\""
    case _ => s
  }

private def needsYamlQuoting(s: scala.Predef.String): Boolean =
  s.exists(c =>
    c == ':' || c == '#' || c == '\n' || c == '"' || c == '\'' || c == '{' || c == '}' || c == '[' ||
      c == ']' || c == ',' || c == '\\'
  )

private def escapeYaml(s: scala.Predef.String): scala.Predef.String =
  s.replace("\\", "\\\\")
    .replace("\"", "\\\"")
    .replace("\n", "\\n")
    .replace("\r", "\\r")
    .replace("\t", "\\t")

private def quoteYamlKey(key: scala.Predef.String): scala.Predef.String =
  key match {
    case "true" | "false" | "null" | "~" | "" => s"\"$key\""
    case _ if needsYamlQuoting(key) || key.contains(' ') => s"\"${escapeYaml(key)}\""
    case _ => key
  }
