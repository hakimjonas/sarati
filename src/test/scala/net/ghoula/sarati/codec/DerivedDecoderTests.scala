package net.ghoula.sarati.codec

import munit.FunSuite

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue
import net.ghoula.sarati.ast.toml.TomlValue
import net.ghoula.sarati.ast.xml.{XmlNode, qname}
import net.ghoula.sarati.ast.yaml.YamlValue

class DerivedDecoderTests extends FunSuite {

  test("TOML: derive decoder for simple case class") {
    import TomlDecoders.given

    case class Point(x: Int, y: Int)
    given Decoder[TomlValue, Point] = Decoder.derived

    val toml = TomlValue.InlineTable(
      Map(
        "x" -> TomlValue.Integer(10L),
        "y" -> TomlValue.Integer(20L)
      )
    )

    val result = Decoder[TomlValue, Point].decode(toml)
    assertEquals(result, Result.Success(Point(10, 20), 0))
  }

  test("TOML: derive decoder for case class with String and Boolean") {
    import TomlDecoders.given

    case class Config(name: String, enabled: Boolean)
    given Decoder[TomlValue, Config] = Decoder.derived

    val toml = TomlValue.InlineTable(
      Map(
        "name" -> TomlValue.String("myapp"),
        "enabled" -> TomlValue.Boolean(true)
      )
    )

    val result = Decoder[TomlValue, Config].decode(toml)
    assertEquals(result, Result.Success(Config("myapp", true), 0))
  }

  test("TOML: derive decoder for nested case class") {
    import TomlDecoders.given

    case class Inner(value: Int)
    case class Outer(name: String, inner: Inner)

    given Decoder[TomlValue, Inner] = Decoder.derived
    given Decoder[TomlValue, Outer] = Decoder.derived

    val toml = TomlValue.InlineTable(
      Map(
        "name" -> TomlValue.String("outer"),
        "inner" -> TomlValue.InlineTable(
          Map("value" -> TomlValue.Integer(42L))
        )
      )
    )

    val result = Decoder[TomlValue, Outer].decode(toml)
    assertEquals(result, Result.Success(Outer("outer", Inner(42)), 0))
  }

  test("TOML: derive decoder with List field") {
    import TomlDecoders.given

    case class Numbers(values: List[Int])
    given Decoder[TomlValue, Numbers] = Decoder.derived

    val toml = TomlValue.InlineTable(
      Map(
        "values" -> TomlValue.Array(
          List(
            TomlValue.Integer(1L),
            TomlValue.Integer(2L),
            TomlValue.Integer(3L)
          )
        )
      )
    )

    val result = Decoder[TomlValue, Numbers].decode(toml)
    assertEquals(result, Result.Success(Numbers(List(1, 2, 3)), 0))
  }

  test("TOML: missing field error") {
    import TomlDecoders.given

    case class Required(name: String, value: Int)
    given Decoder[TomlValue, Required] = Decoder.derived

    val toml = TomlValue.InlineTable(
      Map("name" -> TomlValue.String("test"))
    )

    val result = Decoder[TomlValue, Required].decode(toml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.MissingField("value", _) => true
          case _ => false
        })
      case _ => fail("Expected Failure with missing field error")
    }
  }

  test("TOML: derive decoder with Option field (Some)") {
    import TomlDecoders.given

    case class MaybeValue(name: String, value: Option[Int])
    given Decoder[TomlValue, MaybeValue] = Decoder.derived

    val toml = TomlValue.InlineTable(
      Map(
        "name" -> TomlValue.String("test"),
        "value" -> TomlValue.Integer(42L)
      )
    )

    val result = Decoder[TomlValue, MaybeValue].decode(toml)
    assertEquals(result, Result.Success(MaybeValue("test", Some(42)), 0))
  }

  test("TOML: derive decoder with Option field (None via missing key)") {
    import TomlDecoders.given

    case class MaybeValue(name: String, value: Option[Int])
    given Decoder[TomlValue, MaybeValue] = Decoder.derived

    val toml = TomlValue.InlineTable(
      Map("name" -> TomlValue.String("test"))
    )

    val result = Decoder[TomlValue, MaybeValue].decode(toml)
    assertEquals(result, Result.Success(MaybeValue("test", None), 0))
  }

  test("TOML: type mismatch - InlineTable expected") {
    import TomlDecoders.given

    case class Simple(x: Int)
    given Decoder[TomlValue, Simple] = Decoder.derived

    val toml = TomlValue.String("not a table")
    val result = Decoder[TomlValue, Simple].decode(toml)

    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("InlineTable", "String", _) => true
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("YAML: derive decoder for simple case class") {
    import YamlDecoders.given

    case class Point(x: Int, y: Int)
    given Decoder[YamlValue, Point] = Decoder.derived

    val yaml = YamlValue.Mapping(
      Map(
        "x" -> YamlValue.Integer(10L),
        "y" -> YamlValue.Integer(20L)
      )
    )

    val result = Decoder[YamlValue, Point].decode(yaml)
    assertEquals(result, Result.Success(Point(10, 20), 0))
  }

  test("YAML: derive decoder for case class with String and Boolean") {
    import YamlDecoders.given

    case class Config(name: String, enabled: Boolean)
    given Decoder[YamlValue, Config] = Decoder.derived

    val yaml = YamlValue.Mapping(
      Map(
        "name" -> YamlValue.String("myapp"),
        "enabled" -> YamlValue.Boolean(true)
      )
    )

    val result = Decoder[YamlValue, Config].decode(yaml)
    assertEquals(result, Result.Success(Config("myapp", true), 0))
  }

  test("YAML: derive decoder for nested case class") {
    import YamlDecoders.given

    case class Inner(value: Int)
    case class Outer(name: String, inner: Inner)

    given Decoder[YamlValue, Inner] = Decoder.derived
    given Decoder[YamlValue, Outer] = Decoder.derived

    val yaml = YamlValue.Mapping(
      Map(
        "name" -> YamlValue.String("outer"),
        "inner" -> YamlValue.Mapping(
          Map("value" -> YamlValue.Integer(42L))
        )
      )
    )

    val result = Decoder[YamlValue, Outer].decode(yaml)
    assertEquals(result, Result.Success(Outer("outer", Inner(42)), 0))
  }

  test("YAML: derive decoder with List field") {
    import YamlDecoders.given

    case class Numbers(values: List[Int])
    given Decoder[YamlValue, Numbers] = Decoder.derived

    val yaml = YamlValue.Mapping(
      Map(
        "values" -> YamlValue.Sequence(
          List(
            YamlValue.Integer(1L),
            YamlValue.Integer(2L),
            YamlValue.Integer(3L)
          )
        )
      )
    )

    val result = Decoder[YamlValue, Numbers].decode(yaml)
    assertEquals(result, Result.Success(Numbers(List(1, 2, 3)), 0))
  }

  test("YAML: derive decoder with Option field (Some)") {
    import YamlDecoders.given

    case class MaybeValue(name: String, value: Option[Int])
    given Decoder[YamlValue, MaybeValue] = Decoder.derived

    val yaml = YamlValue.Mapping(
      Map(
        "name" -> YamlValue.String("test"),
        "value" -> YamlValue.Integer(42L)
      )
    )

    val result = Decoder[YamlValue, MaybeValue].decode(yaml)
    assertEquals(result, Result.Success(MaybeValue("test", Some(42)), 0))
  }

  test("YAML: derive decoder with Option field (None via Null)") {
    import YamlDecoders.given

    case class MaybeValue(name: String, value: Option[Int])
    given Decoder[YamlValue, MaybeValue] = Decoder.derived

    val yaml = YamlValue.Mapping(
      Map(
        "name" -> YamlValue.String("test"),
        "value" -> YamlValue.Null
      )
    )

    val result = Decoder[YamlValue, MaybeValue].decode(yaml)
    assertEquals(result, Result.Success(MaybeValue("test", None), 0))
  }

  test("YAML: derive decoder with Option field (None via missing key)") {
    import YamlDecoders.given

    case class MaybeValue(name: String, value: Option[Int])
    given Decoder[YamlValue, MaybeValue] = Decoder.derived

    val yaml = YamlValue.Mapping(
      Map("name" -> YamlValue.String("test"))
    )

    val result = Decoder[YamlValue, MaybeValue].decode(yaml)
    assertEquals(result, Result.Success(MaybeValue("test", None), 0))
  }

  test("YAML: missing field error") {
    import YamlDecoders.given

    case class Required(name: String, value: Int)
    given Decoder[YamlValue, Required] = Decoder.derived

    val yaml = YamlValue.Mapping(
      Map("name" -> YamlValue.String("test"))
    )

    val result = Decoder[YamlValue, Required].decode(yaml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.MissingField("value", _) => true
          case _ => false
        })
      case _ => fail("Expected Failure with missing field error")
    }
  }

  test("YAML: type mismatch - Mapping expected") {
    import YamlDecoders.given

    case class Simple(x: Int)
    given Decoder[YamlValue, Simple] = Decoder.derived

    val yaml = YamlValue.String("not a mapping")
    val result = Decoder[YamlValue, Simple].decode(yaml)

    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Mapping", "String", _) => true
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("XML: derive decoder for simple case class") {
    import XmlDecoders.given

    case class Point(x: Int, y: Int)
    given Decoder[XmlNode, Point] = Decoder.derived

    val xml = XmlNode.Element(
      qname("point"),
      List.empty,
      List(
        XmlNode.Element(qname("x"), List.empty, List(XmlNode.Text("10"))),
        XmlNode.Element(qname("y"), List.empty, List(XmlNode.Text("20")))
      )
    )

    val result = Decoder[XmlNode, Point].decode(xml)
    assertEquals(result, Result.Success(Point(10, 20), 0))
  }

  test("XML: derive decoder for case class with String and Boolean") {
    import XmlDecoders.given

    case class Config(name: String, enabled: Boolean)
    given Decoder[XmlNode, Config] = Decoder.derived

    val xml = XmlNode.Element(
      qname("config"),
      List.empty,
      List(
        XmlNode.Element(qname("name"), List.empty, List(XmlNode.Text("myapp"))),
        XmlNode.Element(qname("enabled"), List.empty, List(XmlNode.Text("true")))
      )
    )

    val result = Decoder[XmlNode, Config].decode(xml)
    assertEquals(result, Result.Success(Config("myapp", true), 0))
  }

  test("XML: derive decoder for nested case class") {
    import XmlDecoders.given

    case class Inner(value: Int)
    case class Outer(name: String, inner: Inner)

    given Decoder[XmlNode, Inner] = Decoder.derived
    given Decoder[XmlNode, Outer] = Decoder.derived

    val xml = XmlNode.Element(
      qname("outer"),
      List.empty,
      List(
        XmlNode.Element(qname("name"), List.empty, List(XmlNode.Text("outer"))),
        XmlNode.Element(
          qname("inner"),
          List.empty,
          List(
            XmlNode.Element(qname("value"), List.empty, List(XmlNode.Text("42")))
          )
        )
      )
    )

    val result = Decoder[XmlNode, Outer].decode(xml)
    assertEquals(result, Result.Success(Outer("outer", Inner(42)), 0))
  }

  test("XML: derive decoder with List field") {
    import XmlDecoders.given

    case class Numbers(values: List[Int])
    given Decoder[XmlNode, Numbers] = Decoder.derived

    val xml = XmlNode.Element(
      qname("numbers"),
      List.empty,
      List(
        XmlNode.Element(
          qname("values"),
          List.empty,
          List(
            XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("1"))),
            XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("2"))),
            XmlNode.Element(qname("item"), List.empty, List(XmlNode.Text("3")))
          )
        )
      )
    )

    val result = Decoder[XmlNode, Numbers].decode(xml)
    assertEquals(result, Result.Success(Numbers(List(1, 2, 3)), 0))
  }

  test("XML: missing field error") {
    import XmlDecoders.given

    case class Required(name: String, value: Int)
    given Decoder[XmlNode, Required] = Decoder.derived

    val xml = XmlNode.Element(
      qname("required"),
      List.empty,
      List(
        XmlNode.Element(qname("name"), List.empty, List(XmlNode.Text("test")))
      )
    )

    val result = Decoder[XmlNode, Required].decode(xml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.MissingField("value", _) => true
          case _ => false
        })
      case _ => fail("Expected Failure with missing field error")
    }
  }

  test("XML: derive decoder with Option field (Some)") {
    import XmlDecoders.given

    case class MaybeValue(name: String, value: Option[Int])
    given Decoder[XmlNode, MaybeValue] = Decoder.derived

    val xml = XmlNode.Element(
      qname("item"),
      List.empty,
      List(
        XmlNode.Element(qname("name"), List.empty, List(XmlNode.Text("test"))),
        XmlNode.Element(qname("value"), List.empty, List(XmlNode.Text("42")))
      )
    )

    val result = Decoder[XmlNode, MaybeValue].decode(xml)
    assertEquals(result, Result.Success(MaybeValue("test", Some(42)), 0))
  }

  test("XML: derive decoder with Option field (None via missing child)") {
    import XmlDecoders.given

    case class MaybeValue(name: String, value: Option[Int])
    given Decoder[XmlNode, MaybeValue] = Decoder.derived

    val xml = XmlNode.Element(
      qname("item"),
      List.empty,
      List(
        XmlNode.Element(qname("name"), List.empty, List(XmlNode.Text("test")))
      )
    )

    val result = Decoder[XmlNode, MaybeValue].decode(xml)
    assertEquals(result, Result.Success(MaybeValue("test", None), 0))
  }

  test("XML: type mismatch - Element expected") {
    import XmlDecoders.given

    case class Simple(x: Int)
    given Decoder[XmlNode, Simple] = Decoder.derived

    val xml = XmlNode.Text("not an element")
    val result = Decoder[XmlNode, Simple].decode(xml)

    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Element", "Text", _) => true
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("same case class can be decoded from all formats") {
    import JsonDecoders.given
    import TomlDecoders.given
    import YamlDecoders.given
    import XmlDecoders.given

    case class Person(name: String, age: Int)

    given Decoder[JsonValue, Person] = Decoder.derived
    given Decoder[TomlValue, Person] = Decoder.derived
    given Decoder[YamlValue, Person] = Decoder.derived
    given Decoder[XmlNode, Person] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Alice"),
        "age" -> JsonValue.Number(30.0)
      )
    )

    val toml = TomlValue.InlineTable(
      Map(
        "name" -> TomlValue.String("Alice"),
        "age" -> TomlValue.Integer(30L)
      )
    )

    val yaml = YamlValue.Mapping(
      Map(
        "name" -> YamlValue.String("Alice"),
        "age" -> YamlValue.Integer(30L)
      )
    )

    val xml = XmlNode.Element(
      qname("person"),
      List.empty,
      List(
        XmlNode.Element(qname("name"), List.empty, List(XmlNode.Text("Alice"))),
        XmlNode.Element(qname("age"), List.empty, List(XmlNode.Text("30")))
      )
    )

    val expected = Person("Alice", 30)

    assertEquals(Decoder[JsonValue, Person].decode(json), Result.Success(expected, 0))
    assertEquals(Decoder[TomlValue, Person].decode(toml), Result.Success(expected, 0))
    assertEquals(Decoder[YamlValue, Person].decode(yaml), Result.Success(expected, 0))
    assertEquals(Decoder[XmlNode, Person].decode(xml), Result.Success(expected, 0))
  }
}
