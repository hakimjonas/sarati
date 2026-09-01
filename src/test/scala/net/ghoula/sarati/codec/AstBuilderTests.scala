package net.ghoula.sarati.codec

import munit.FunSuite

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue
import net.ghoula.sarati.ast.toml.TomlValue
import net.ghoula.sarati.ast.yaml.YamlValue

class AstBuilderTests extends FunSuite {

  import JsonDecoders.given
  import JsonEncoders.given

  case class Point(x: Int, y: Int)
  given Encoder[Point, JsonValue] = Encoder.derived
  given Decoder[JsonValue, Point] = Decoder.derived

  case class Named(name: String, value: Double)
  given Encoder[Named, JsonValue] = Encoder.derived
  given Decoder[JsonValue, Named] = Decoder.derived

  test("JSON round-trip via AstBuilder-based Encoder") {
    val point = Point(10, 20)
    val encoded = Encoder[Point, JsonValue].encode(point)
    val decoded = Decoder[JsonValue, Point].decode(encoded)
    decoded match {
      case Result.Success(value, _) => assertEquals(value, point)
      case other => fail(s"Expected Success, got $other")
    }
  }

  test("JSON round-trip with String and Double fields") {
    val named = Named("test", 3.14)
    val encoded = Encoder[Named, JsonValue].encode(named)
    val decoded = Decoder[JsonValue, Named].decode(encoded)
    decoded match {
      case Result.Success(value, _) => assertEquals(value, named)
      case other => fail(s"Expected Success, got $other")
    }
  }

  test("AstBuilder[JsonValue] creates correct structure") {
    val builder = summon[AstBuilder[JsonValue]]
    assertEquals(builder.fromString("hello"), JsonValue.Str("hello"))
    assertEquals(builder.fromNumber(42.0), JsonValue.Number(42.0))
    assertEquals(builder.fromBoolean(true), JsonValue.Bool(true))
    assertEquals(builder.fromNull, JsonValue.Null)
    assertEquals(builder.createArray(List(JsonValue.Number(1.0))), JsonValue.Array(List(JsonValue.Number(1.0))))
    assertEquals(
      builder.createObject(Map("a" -> JsonValue.Number(1.0))),
      JsonValue.Object(Map("a" -> JsonValue.Number(1.0)))
    )
  }

  test("AstBuilder[YamlValue] creates correct structure") {
    val builder = summon[AstBuilder[YamlValue]]
    assertEquals(builder.fromString("hello"), YamlValue.String("hello"))
    assertEquals(builder.fromNumber(42.0), YamlValue.Float(42.0))
    assertEquals(builder.fromBoolean(true), YamlValue.Boolean(true))
    assertEquals(builder.fromNull, YamlValue.Null)
    assertEquals(
      builder.createArray(List(YamlValue.String("a"))),
      YamlValue.Sequence(List(YamlValue.String("a")))
    )
    assertEquals(
      builder.createObject(Map("k" -> YamlValue.String("v"))),
      YamlValue.Mapping(Map("k" -> YamlValue.String("v")))
    )
  }

  test("AstBuilder[TomlValue] creates correct structure") {
    val builder = summon[AstBuilder[TomlValue]]
    assertEquals(builder.fromString("hello"), TomlValue.String("hello"))
    assertEquals(builder.fromNumber(42.0), TomlValue.Float(42.0))
    assertEquals(builder.fromBoolean(true), TomlValue.Boolean(true))
    assertEquals(
      builder.createArray(List(TomlValue.String("a"))),
      TomlValue.Array(List(TomlValue.String("a")))
    )
    assertEquals(
      builder.createObject(Map("k" -> TomlValue.String("v"))),
      TomlValue.InlineTable(Map("k" -> TomlValue.String("v")))
    )
  }
}
