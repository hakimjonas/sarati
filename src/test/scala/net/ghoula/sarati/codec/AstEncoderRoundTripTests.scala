package net.ghoula.sarati.codec

import munit.FunSuite

import net.ghoula.sarati.ast.toml.TomlValue
import net.ghoula.sarati.ast.xml.XmlNode
import net.ghoula.sarati.ast.yaml.YamlValue
import net.ghoula.sarati.{DecodeError, Result}

/** Round-trips through the AST encoder and decoder layers for the non-JSON formats, pinning the
  * AstBuilder conventions the encoders follow — including the lossy cases documented on the encoder
  * objects.
  */
class AstEncoderRoundTripTests extends FunSuite {

  case class Record(id: Long, name: String, active: Boolean, note: Option[Int])

  private val record = Record(7, "x", true, Some(3))

  test("TOML: derive encode and decode round-trip") {
    import TomlEncoders.given
    import TomlDecoders.given

    given Encoder[Record, TomlValue] = Encoder.derived
    given Decoder[TomlValue, Record] = Decoder.derived

    val ast = Encoder[Record, TomlValue].encode(record)
    assertEquals(Decoder[TomlValue, Record].decode(ast), Result.Success(record, 0))
  }

  test("TOML: None encodes as an empty string and decodes back as an empty Some — no TOML null") {
    import TomlEncoders.given
    import TomlDecoders.given

    given Encoder[Record, TomlValue] = Encoder.derived
    given Decoder[TomlValue, Record] = Decoder.derived

    val ast = Encoder[Record, TomlValue].encode(record.copy(note = None))
    val meta = ast match {
      case TomlValue.InlineTable(fields) => fields("note")
      case other => fail(s"expected InlineTable, got $other")
    }
    assertEquals(meta, TomlValue.String(""))
    // "" is not an integer — the loss surfaces as a decode failure, not silent corruption
    Decoder[TomlValue, Record].decode(ast) match {
      case Result.Failure(_, _) => () // "" is not an integer — the loss is a failure, not corruption
      case other => fail(s"expected Failure for the lossy None encoding, got $other")
    }
  }

  test("TOML: Byte, Short, BigInt, BigDecimal decoders") {
    import TomlDecoders.given

    assertEquals(Decoder[TomlValue, Byte].decode(TomlValue.Integer(127)), Result.Success(127.toByte, 0))
    Decoder[TomlValue, Byte].decode(TomlValue.Integer(128)) match {
      case Result.Failure(_, _) => ()
      case other => fail(s"expected failure, got $other")
    }
    assertEquals(Decoder[TomlValue, Short].decode(TomlValue.Integer(-32768)), Result.Success((-32768).toShort, 0))
    assertEquals(Decoder[TomlValue, BigInt].decode(TomlValue.Integer(-1)), Result.Success(BigInt(-1), 0))
    assertEquals(Decoder[TomlValue, BigDecimal].decode(TomlValue.Integer(3)), Result.Success(BigDecimal(3), 0))
    assertEquals(Decoder[TomlValue, BigDecimal].decode(TomlValue.Float(2.5)), Result.Success(BigDecimal(2.5), 0))
  }

  test("YAML: derive encode and decode round-trip") {
    import YamlEncoders.given
    import YamlDecoders.given

    given Encoder[Record, YamlValue] = Encoder.derived
    given Decoder[YamlValue, Record] = Decoder.derived

    val ast = Encoder[Record, YamlValue].encode(record)
    assertEquals(Decoder[YamlValue, Record].decode(ast), Result.Success(record, 0))
  }

  test("YAML: None round-trips through Null") {
    import YamlEncoders.given
    import YamlDecoders.given

    given Encoder[Record, YamlValue] = Encoder.derived
    given Decoder[YamlValue, Record] = Decoder.derived

    val ast = Encoder[Record, YamlValue].encode(record.copy(note = None))
    assertEquals(Decoder[YamlValue, Record].decode(ast), Result.Success(record.copy(note = None), 0))
  }

  test("XML: derive encode and decode round-trip") {
    import XmlEncoders.given
    import XmlDecoders.given

    given Encoder[Record, XmlNode] = Encoder.derived
    given Decoder[XmlNode, Record] = Decoder.derived

    val ast = Encoder[Record, XmlNode].encode(record)
    assertEquals(Decoder[XmlNode, Record].decode(ast), Result.Success(record, 0))
  }

  test("XML: Option[Int] with None fails to decode — empty text is not an integer (documented)") {
    import XmlEncoders.given
    import XmlDecoders.given

    given Encoder[Record, XmlNode] = Encoder.derived
    given Decoder[XmlNode, Record] = Decoder.derived

    val ast = Encoder[Record, XmlNode].encode(record.copy(note = None))
    Decoder[XmlNode, Record].decode(ast) match {
      case Result.Failure(_, _) => ()
      case other => fail(s"expected Failure for the lossy None encoding, got $other")
    }
  }

  test("XML: Option[String] None decodes back as an empty Some — empty text is ambiguous (documented)") {
    import XmlEncoders.given
    import XmlDecoders.given

    case class Named(name: Option[String])
    given Encoder[Named, XmlNode] = Encoder.derived
    given Decoder[XmlNode, Named] = Decoder.derived

    val ast = Encoder[Named, XmlNode].encode(Named(None))
    assertEquals(Decoder[XmlNode, Named].decode(ast), Result.Success(Named(Some("")), 0))
  }

  test("XML: standalone List of case classes round-trips through the array shape") {
    import XmlEncoders.given
    import XmlDecoders.given

    case class Item(id: Int)
    given Encoder[Item, XmlNode] = Encoder.derived
    given Decoder[XmlNode, Item] = Decoder.derived

    val source = List(Item(1), Item(2))
    val ast = Encoder[List[Item], XmlNode].encode(source)
    assertEquals(Decoder[XmlNode, List[Item]].decode(ast), Result.Success(source, 0))
  }

  test("XML: a collection field does not round-trip — the field element and the array wrapper nest (documented)") {
    import XmlEncoders.given
    import XmlDecoders.given

    case class Item(id: Int)
    case class Holder(items: List[Item])
    given Encoder[Item, XmlNode] = Encoder.derived
    given Decoder[XmlNode, Item] = Decoder.derived
    given Encoder[Holder, XmlNode] = Encoder.derived
    given Decoder[XmlNode, Holder] = Decoder.derived

    val ast = Encoder[Holder, XmlNode].encode(Holder(List(Item(1))))
    // the field element <items> wraps the <array> wrapper; the list decoder reads <array> as an Item
    Decoder[XmlNode, Holder].decode(ast) match {
      case Result.Failure(DecodeError.MissingField("id", _) :: Nil, _) => ()
      case other => fail(s"expected the documented MissingField failure, got $other")
    }
  }

  test("XML: map encoder and decoder round-trip through the object shape") {
    import XmlEncoders.given
    import XmlDecoders.given

    val source = Map("a" -> 1, "b" -> 2)
    val ast = Encoder[Map[String, Int], XmlNode].encode(source)
    assertEquals(Decoder[XmlNode, Map[String, Int]].decode(ast), Result.Success(source, 0))
  }
}
