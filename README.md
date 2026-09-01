# Sarati

Sarati is a binary codec library and structural AST layer for Scala 3, with an XPath 1.0 evaluator over its XML AST. It has four parts:

1. **Binary codecs** (`net.ghoula.sarati.SaratiCodec`): offset-based binary encoding and decoding for primitives, collections, and case classes/enums derived at compile time.
2. **Format ASTs** (`net.ghoula.sarati.ast.*`): value trees for JSON, TOML, YAML, and XML, decoupled from text parsing, plus a formatter for each.
3. **Structural codec layer** (`net.ghoula.sarati.codec`): `Decoder`/`Encoder` typeclasses that map the format ASTs to and from typed Scala data, with derivation for case classes.
4. **XPath** (`net.ghoula.sarati.xpath`): an XPath 1.0 evaluator over the XML AST.

## Installation

Sarati is published to Maven Central:

```scala
libraryDependencies += "net.ghoula" %% "sarati" % "1.0.0-alpha"
```

## Design

1. **Offset-based decoding.** `SaratiCodec.decode` takes an `IArray[Byte]`, an offset, and returns `Either[SaratiError, (A, Int)]` — the decoded value and the number of bytes consumed. Decoding a field out of a larger buffer leaves the caller at the next field.
2. **Compile-time derivation.** Product and sum codecs are derived from Mirrors by inline metaprogramming (`SaratiCodec.derived`): the expansion emits one encode/decode call per field or per enum case and reports missing codecs at compile time, with the field names and the missing instance types. There is no runtime reflection.
3. **No runtime dependencies.** The main scope has no `libraryDependencies`; everything outside tests uses the Scala 3 standard library only.
4. **Explicit memory operations.** Wire assembly uses `System.arraycopy` and `Array` arithmetic (see `ByteOps`), with `IArray[Byte]` as the public byte type.

## Binary wire format

Products encode as the concatenation of their field encodings in declaration order; enums encode as the case ordinal followed by that case's payload. No field names, case names, or tags appear on the wire, so renaming or reordering fields or cases changes the format; appending new enum cases at the end keeps earlier ordinals stable.

| Type | Encoding |
|---|---|
| `Long`, `Int`, `Short` | ZigZag-mapped, then LEB128 varint |
| `Byte` | 1 raw byte |
| `Boolean` | 1 byte: `0x00` false, `0x01` true (any other value fails decode) |
| `Double`, `Float` | 8 / 4 bytes, IEEE 754 bits, big-endian (`doubleToLongBits`/`floatToIntBits`, so NaN values canonicalize) |
| `String` | LEB128 varint byte length, then UTF-8 bytes |
| `Option[A]` | 1 tag byte: `0x00` none, `0x01` then `A` |
| `List[A]` | LEB128 varint element count, then the elements |
| `IArray[Byte]` | LEB128 varint length, then the bytes |

Decode failures return `SaratiError`: `Eof` with the position reached, `VarintOverflow` past 64 bits, or `ParseError` for invalid tags and ordinals.

```scala
import net.ghoula.sarati.*

case class DomainState(etag: String, nextFetch: Long) derives SaratiCodec

val state = DomainState("abc-123", 1709420400000L)

// Encodes to an IArray[Byte]
val bytes: IArray[Byte] = summon[SaratiCodec[DomainState]].encode(state)

// Decodes at an offset, returning the value and the bytes consumed
val decoded = summon[SaratiCodec[DomainState]].decode(bytes, offset = 0)
// Right((DomainState("abc-123", 1709420400000L), 14))
```

(The 14 bytes are the 1-byte length prefix plus 7 UTF-8 bytes for `abc-123`, and 6 ZigZag-varint bytes for the `Long`.)

### Enum derivation

Enum codecs derive from `Mirror.SumOf`. The ordinal encodes with the `Int` codec (ZigZag varint), then the case payload encodes like a product. A decode whose ordinal is out of range fails with `SaratiError.ParseError`.

```scala
enum LogicalPlan:
  case Filter(predicate: String)
  case Limit(n: Int)
  case Union(left: LogicalPlan, right: LogicalPlan)

object LogicalPlan:
  given SaratiCodec[LogicalPlan.Filter] = SaratiCodec.derived
  given SaratiCodec[LogicalPlan.Limit] = SaratiCodec.derived
  given SaratiCodec[LogicalPlan.Union] = SaratiCodec.derived
  given SaratiCodec[LogicalPlan] = SaratiCodec.derived

val plan = LogicalPlan.Union(LogicalPlan.Limit(100), LogicalPlan.Filter("age > 18"))
val payload: IArray[Byte] = summon[SaratiCodec[LogicalPlan]].encode(plan)
```

## The AST layer

Sarati defines the value trees that text parsers produce and codecs consume:

* `net.ghoula.sarati.ast.json.JsonValue`
* `net.ghoula.sarati.ast.toml.TomlValue` (+ `TomlTable`/`TomlDocument` for table structure)
* `net.ghoula.sarati.ast.yaml.YamlValue`
* `net.ghoula.sarati.ast.xml.XmlNode` (+ `XmlDocument`, `XmlConfig` presets)

Each package provides a formatter (`formatJson`, `formatToml`, `formatYaml`, `formatXml`/`formatXmlDocument`); text parsing itself is out of sarati's scope, which keeps the trees decoupled from any one parser.

## The structural codec layer

`Decoder[From, To]` maps an AST node to a typed value; `Encoder[A, To]` maps typed data back. `AstStruct` instances tell the derivation how to read fields out of each AST (JSON objects, TOML inline tables, YAML mappings, XML elements), and `AstBuilder` instances how to construct them — this is what `Decoder.derived`/`Encoder.derived` build on.

Details worth knowing before use:

* Decoding is stack-safe: derived and composite decoders run as `Eval` trampolines, so nesting depth is bounded by heap. The 10,000-deep decode benchmark runs without `StackOverflowError` (see `benchmarks/results`).
* `FieldTransformer` renames or excludes fields at the AST boundary; the module ships `SnakeCase`, `KebabCase`, and `ScreamingSnakeCase`. An excluded field decodes exactly like a missing key: `None` for an `Option` field, `MissingField` error for a required one.
* Numeric decoding is strict: a `JsonValue.Number` decodes to `Int`/`Long`/`Byte`/`Short` only when it is whole and in range; otherwise the decode fails with `TypeMismatch`. (`Long` *encoding* to JSON goes through `Double` and loses precision past 2^53.)

## XPath

`net.ghoula.sarati.xpath` evaluates XPath 1.0 expressions over `XmlNode` documents: all 13 axes, predicates with proximity positions, the full operator ladder, and the 27-function core library. `wrapDocument` builds the immutable node model (document-order indices, parent/ancestor navigation) from a parsed document; `xpathXmlConfig` is the `XmlConfig` preset XPath evaluation needs (whitespace preserved, comments/PIs/entities on).

Documented divergences: `id()` and `namespace-uri()` return empty results, the namespace axis is empty, and variable references evaluate to `Unsupported` errors. Evaluation errors are values (`Either[XPathError, XPathValue]`), not exceptions.
