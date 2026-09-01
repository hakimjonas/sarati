package net.ghoula.sarati.codec

import munit.FunSuite

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.yaml.YamlValue

class YamlDecoderTests extends FunSuite {

  import YamlDecoders.given

  test("decode YAML string to String") {
    val yaml = YamlValue.String("hello")
    val result = Decoder[YamlValue, String].decode(yaml)
    assertEquals(result, Result.Success("hello", 0))
  }

  test("decode YAML integer to String (coercion)") {
    val yaml = YamlValue.Integer(42L)
    val result = Decoder[YamlValue, String].decode(yaml)
    assertEquals(result, Result.Success("42", 0))
  }

  test("decode YAML float to String (coercion)") {
    val yaml = YamlValue.Float(3.14)
    val result = Decoder[YamlValue, String].decode(yaml)
    assertEquals(result, Result.Success("3.14", 0))
  }

  test("decode YAML boolean to String (coercion)") {
    val yaml = YamlValue.Boolean(true)
    val result = Decoder[YamlValue, String].decode(yaml)
    assertEquals(result, Result.Success("true", 0))
  }

  test("decode YAML null to String (coercion)") {
    val yaml = YamlValue.Null
    val result = Decoder[YamlValue, String].decode(yaml)
    assertEquals(result, Result.Success("null", 0))
  }

  test("decode YAML integer to Int") {
    val yaml = YamlValue.Integer(42L)
    val result = Decoder[YamlValue, Int].decode(yaml)
    assertEquals(result, Result.Success(42, 0))
  }

  test("decode YAML integer to Long") {
    val yaml = YamlValue.Integer(9876543210L)
    val result = Decoder[YamlValue, Long].decode(yaml)
    assertEquals(result, Result.Success(9876543210L, 0))
  }

  test("decode YAML float to Double") {
    val yaml = YamlValue.Float(3.14159)
    val result = Decoder[YamlValue, Double].decode(yaml)
    assertEquals(result, Result.Success(3.14159, 0))
  }

  test("decode YAML integer to Double (promotion)") {
    val yaml = YamlValue.Integer(42L)
    val result = Decoder[YamlValue, Double].decode(yaml)
    assertEquals(result, Result.Success(42.0, 0))
  }

  test("decode YAML float to Float") {
    val yaml = YamlValue.Float(1.5)
    val result = Decoder[YamlValue, Float].decode(yaml)
    assertEquals(result, Result.Success(1.5f, 0))
  }

  test("decode YAML integer to Float (promotion)") {
    val yaml = YamlValue.Integer(42L)
    val result = Decoder[YamlValue, Float].decode(yaml)
    assertEquals(result, Result.Success(42.0f, 0))
  }

  test("decode YAML boolean to Boolean") {
    val yamlTrue = YamlValue.Boolean(true)
    val yamlFalse = YamlValue.Boolean(false)
    assertEquals(Decoder[YamlValue, Boolean].decode(yamlTrue), Result.Success(true, 0))
    assertEquals(Decoder[YamlValue, Boolean].decode(yamlFalse), Result.Success(false, 0))
  }

  test("decode YAML null to Option[String]") {
    val yaml = YamlValue.Null
    val result = Decoder[YamlValue, Option[String]].decode(yaml)
    assertEquals(result, Result.Success(None, 0))
  }

  test("decode YAML null to Option[Int]") {
    val yaml = YamlValue.Null
    val result = Decoder[YamlValue, Option[Int]].decode(yaml)
    assertEquals(result, Result.Success(None, 0))
  }

  test("decode YAML value to Option[String]") {
    val yaml = YamlValue.String("hello")
    val result = Decoder[YamlValue, Option[String]].decode(yaml)
    assertEquals(result, Result.Success(Some("hello"), 0))
  }

  test("decode YAML value to Option[Int]") {
    val yaml = YamlValue.Integer(42L)
    val result = Decoder[YamlValue, Option[Int]].decode(yaml)
    assertEquals(result, Result.Success(Some(42), 0))
  }

  test("decode YAML sequence to List[Int]") {
    val yaml = YamlValue.Sequence(
      List(
        YamlValue.Integer(1L),
        YamlValue.Integer(2L),
        YamlValue.Integer(3L)
      )
    )
    val result = Decoder[YamlValue, List[Int]].decode(yaml)
    assertEquals(result, Result.Success(List(1, 2, 3), 0))
  }

  test("decode YAML sequence to List[String]") {
    val yaml = YamlValue.Sequence(
      List(
        YamlValue.String("apple"),
        YamlValue.String("banana"),
        YamlValue.String("cherry")
      )
    )
    val result = Decoder[YamlValue, List[String]].decode(yaml)
    assertEquals(result, Result.Success(List("apple", "banana", "cherry"), 0))
  }

  test("decode YAML sequence to Seq[Int]") {
    val yaml = YamlValue.Sequence(
      List(
        YamlValue.Integer(1L),
        YamlValue.Integer(2L)
      )
    )
    val result = Decoder[YamlValue, Seq[Int]].decode(yaml)
    assertEquals(result, Result.Success(Seq(1, 2), 0))
  }

  test("decode YAML sequence to Vector[Boolean]") {
    val yaml = YamlValue.Sequence(
      List(
        YamlValue.Boolean(true),
        YamlValue.Boolean(false)
      )
    )
    val result = Decoder[YamlValue, Vector[Boolean]].decode(yaml)
    assertEquals(result, Result.Success(Vector(true, false), 0))
  }

  test("decode empty YAML sequence") {
    val yaml = YamlValue.Sequence(List.empty)
    val result = Decoder[YamlValue, List[String]].decode(yaml)
    assertEquals(result, Result.Success(List.empty, 0))
  }

  test("decode YAML mapping to Map[String, Int]") {
    val yaml = YamlValue.Mapping(
      Map(
        "a" -> YamlValue.Integer(1L),
        "b" -> YamlValue.Integer(2L),
        "c" -> YamlValue.Integer(3L)
      )
    )
    val result = Decoder[YamlValue, Map[String, Int]].decode(yaml)
    assertEquals(result, Result.Success(Map("a" -> 1, "b" -> 2, "c" -> 3), 0))
  }

  test("decode YAML mapping to Map[String, String]") {
    val yaml = YamlValue.Mapping(
      Map(
        "name" -> YamlValue.String("Alice"),
        "email" -> YamlValue.String("alice@example.com")
      )
    )
    val result = Decoder[YamlValue, Map[String, String]].decode(yaml)
    assertEquals(result, Result.Success(Map("name" -> "Alice", "email" -> "alice@example.com"), 0))
  }

  test("decode empty YAML mapping") {
    val yaml = YamlValue.Mapping(Map.empty)
    val result = Decoder[YamlValue, Map[String, String]].decode(yaml)
    assertEquals(result, Result.Success(Map.empty, 0))
  }

  test("decode nested YAML sequences") {
    val yaml = YamlValue.Sequence(
      List(
        YamlValue.Sequence(List(YamlValue.Integer(1L), YamlValue.Integer(2L))),
        YamlValue.Sequence(List(YamlValue.Integer(3L), YamlValue.Integer(4L)))
      )
    )
    val result = Decoder[YamlValue, List[List[Int]]].decode(yaml)
    assertEquals(result, Result.Success(List(List(1, 2), List(3, 4)), 0))
  }

  test("decode YAML sequence with Option elements") {
    val yaml = YamlValue.Sequence(
      List(
        YamlValue.String("hello"),
        YamlValue.Null,
        YamlValue.String("world")
      )
    )
    val result = Decoder[YamlValue, List[Option[String]]].decode(yaml)
    assertEquals(result, Result.Success(List(Some("hello"), None, Some("world")), 0))
  }

  test("type mismatch error - Int expected, String found") {
    val yaml = YamlValue.String("not a number")
    val result = Decoder[YamlValue, Int].decode(yaml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Int", actual, _) => actual.contains("String")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Boolean expected, String found") {
    val yaml = YamlValue.String("not a boolean")
    val result = Decoder[YamlValue, Boolean].decode(yaml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Boolean", actual, _) => actual.contains("String")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Int out of range") {
    val yaml = YamlValue.Integer(Long.MaxValue)
    val result = Decoder[YamlValue, Int].decode(yaml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Int", actual, _) => actual.contains("out of range")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Sequence expected, String found") {
    val yaml = YamlValue.String("not a sequence")
    val result = Decoder[YamlValue, List[Int]].decode(yaml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Sequence", actual, _) => actual.contains("String")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Mapping expected, Sequence found") {
    val yaml = YamlValue.Sequence(List(YamlValue.Integer(1L)))
    val result = Decoder[YamlValue, Map[String, Int]].decode(yaml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Mapping", actual, _) => actual.contains("Sequence")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Long expected, Float found") {
    val yaml = YamlValue.Float(3.14)
    val result = Decoder[YamlValue, Long].decode(yaml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Long", actual, _) => actual.contains("Float")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }
}
