package net.ghoula.sarati.codec

import munit.FunSuite

import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZoneOffset}

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.toml.TomlValue

class TomlDecoderTests extends FunSuite {

  import TomlDecoders.given

  test("decode TOML string to String") {
    val toml = TomlValue.String("hello")
    val result = Decoder[TomlValue, String].decode(toml)
    assertEquals(result, Result.Success("hello", 0))
  }

  test("decode TOML integer to Int") {
    val toml = TomlValue.Integer(42L)
    val result = Decoder[TomlValue, Int].decode(toml)
    assertEquals(result, Result.Success(42, 0))
  }

  test("decode TOML integer to Long") {
    val toml = TomlValue.Integer(9876543210L)
    val result = Decoder[TomlValue, Long].decode(toml)
    assertEquals(result, Result.Success(9876543210L, 0))
  }

  test("decode TOML float to Double") {
    val toml = TomlValue.Float(3.14159)
    val result = Decoder[TomlValue, Double].decode(toml)
    assertEquals(result, Result.Success(3.14159, 0))
  }

  test("decode TOML integer to Double (promotion)") {
    val toml = TomlValue.Integer(42L)
    val result = Decoder[TomlValue, Double].decode(toml)
    assertEquals(result, Result.Success(42.0, 0))
  }

  test("decode TOML float to Float") {
    val toml = TomlValue.Float(1.5)
    val result = Decoder[TomlValue, Float].decode(toml)
    assertEquals(result, Result.Success(1.5f, 0))
  }

  test("decode TOML boolean to Boolean") {
    val tomlTrue = TomlValue.Boolean(true)
    val tomlFalse = TomlValue.Boolean(false)
    assertEquals(Decoder[TomlValue, Boolean].decode(tomlTrue), Result.Success(true, 0))
    assertEquals(Decoder[TomlValue, Boolean].decode(tomlFalse), Result.Success(false, 0))
  }

  test("decode TOML DateTime to OffsetDateTime") {
    val dt = OffsetDateTime.of(2024, 1, 15, 10, 30, 0, 0, ZoneOffset.UTC)
    val toml = TomlValue.DateTime(dt)
    val result = Decoder[TomlValue, OffsetDateTime].decode(toml)
    assertEquals(result, Result.Success(dt, 0))
  }

  test("decode TOML LocalDateTime to LocalDateTime") {
    val ldt = LocalDateTime.of(2024, 1, 15, 10, 30, 0)
    val toml = TomlValue.LocalDateTime(ldt)
    val result = Decoder[TomlValue, LocalDateTime].decode(toml)
    assertEquals(result, Result.Success(ldt, 0))
  }

  test("decode TOML DateTime to LocalDateTime (extraction)") {
    val dt = OffsetDateTime.of(2024, 1, 15, 10, 30, 0, 0, ZoneOffset.UTC)
    val toml = TomlValue.DateTime(dt)
    val result = Decoder[TomlValue, LocalDateTime].decode(toml)
    assertEquals(result, Result.Success(dt.toLocalDateTime, 0))
  }

  test("decode TOML LocalDate to LocalDate") {
    val ld = LocalDate.of(2024, 1, 15)
    val toml = TomlValue.LocalDate(ld)
    val result = Decoder[TomlValue, LocalDate].decode(toml)
    assertEquals(result, Result.Success(ld, 0))
  }

  test("decode TOML LocalDateTime to LocalDate (extraction)") {
    val ldt = LocalDateTime.of(2024, 1, 15, 10, 30, 0)
    val toml = TomlValue.LocalDateTime(ldt)
    val result = Decoder[TomlValue, LocalDate].decode(toml)
    assertEquals(result, Result.Success(ldt.toLocalDate, 0))
  }

  test("decode TOML LocalTime to LocalTime") {
    val lt = LocalTime.of(10, 30, 0)
    val toml = TomlValue.LocalTime(lt)
    val result = Decoder[TomlValue, LocalTime].decode(toml)
    assertEquals(result, Result.Success(lt, 0))
  }

  test("decode TOML LocalDateTime to LocalTime (extraction)") {
    val ldt = LocalDateTime.of(2024, 1, 15, 10, 30, 0)
    val toml = TomlValue.LocalDateTime(ldt)
    val result = Decoder[TomlValue, LocalTime].decode(toml)
    assertEquals(result, Result.Success(ldt.toLocalTime, 0))
  }

  test("decode TOML array to List[Int]") {
    val toml = TomlValue.Array(
      List(
        TomlValue.Integer(1L),
        TomlValue.Integer(2L),
        TomlValue.Integer(3L)
      )
    )
    val result = Decoder[TomlValue, List[Int]].decode(toml)
    assertEquals(result, Result.Success(List(1, 2, 3), 0))
  }

  test("decode TOML array to List[String]") {
    val toml = TomlValue.Array(
      List(
        TomlValue.String("apple"),
        TomlValue.String("banana"),
        TomlValue.String("cherry")
      )
    )
    val result = Decoder[TomlValue, List[String]].decode(toml)
    assertEquals(result, Result.Success(List("apple", "banana", "cherry"), 0))
  }

  test("decode empty TOML array") {
    val toml = TomlValue.Array(List.empty)
    val result = Decoder[TomlValue, List[String]].decode(toml)
    assertEquals(result, Result.Success(List.empty, 0))
  }

  test("decode TOML inline table to Map[String, Int]") {
    val toml = TomlValue.InlineTable(
      Map(
        "a" -> TomlValue.Integer(1L),
        "b" -> TomlValue.Integer(2L),
        "c" -> TomlValue.Integer(3L)
      )
    )
    val result = Decoder[TomlValue, Map[String, Int]].decode(toml)
    assertEquals(result, Result.Success(Map("a" -> 1, "b" -> 2, "c" -> 3), 0))
  }

  test("decode TOML inline table to Map[String, String]") {
    val toml = TomlValue.InlineTable(
      Map(
        "name" -> TomlValue.String("Alice"),
        "email" -> TomlValue.String("alice@example.com")
      )
    )
    val result = Decoder[TomlValue, Map[String, String]].decode(toml)
    assertEquals(result, Result.Success(Map("name" -> "Alice", "email" -> "alice@example.com"), 0))
  }

  test("decode TOML value to Option[String]") {
    val toml = TomlValue.String("hello")
    val result = Decoder[TomlValue, Option[String]].decode(toml)
    assertEquals(result, Result.Success(Some("hello"), 0))
  }

  test("decode TOML value to Option[Int]") {
    val toml = TomlValue.Integer(42L)
    val result = Decoder[TomlValue, Option[Int]].decode(toml)
    assertEquals(result, Result.Success(Some(42), 0))
  }

  test("decode nested TOML arrays") {
    val toml = TomlValue.Array(
      List(
        TomlValue.Array(List(TomlValue.Integer(1L), TomlValue.Integer(2L))),
        TomlValue.Array(List(TomlValue.Integer(3L), TomlValue.Integer(4L)))
      )
    )
    val result = Decoder[TomlValue, List[List[Int]]].decode(toml)
    assertEquals(result, Result.Success(List(List(1, 2), List(3, 4)), 0))
  }

  test("type mismatch error - String expected, Integer found") {
    val toml = TomlValue.Integer(42L)
    val result = Decoder[TomlValue, String].decode(toml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("String", actual, _) => actual.contains("Integer")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Boolean expected, String found") {
    val toml = TomlValue.String("not a boolean")
    val result = Decoder[TomlValue, Boolean].decode(toml)
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
    val toml = TomlValue.Integer(Long.MaxValue)
    val result = Decoder[TomlValue, Int].decode(toml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Int", actual, _) => actual.contains("out of range")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - Array expected, String found") {
    val toml = TomlValue.String("not an array")
    val result = Decoder[TomlValue, List[Int]].decode(toml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Array", actual, _) => actual.contains("String")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - InlineTable expected, Array found") {
    val toml = TomlValue.Array(List(TomlValue.Integer(1L)))
    val result = Decoder[TomlValue, Map[String, Int]].decode(toml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("InlineTable", actual, _) => actual.contains("Array")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - OffsetDateTime expected, String found") {
    val toml = TomlValue.String("not a datetime")
    val result = Decoder[TomlValue, OffsetDateTime].decode(toml)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("OffsetDateTime", actual, _) => actual.contains("String")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  // ==== Document-level decoding: TomlTable -> toInlineValue -> derived decoder ====

  test("subtables decode through toInlineValue into nested case classes") {
    import net.ghoula.sarati.ast.toml.{TomlTable, toInlineValue}

    case class Host(name: String, port: Int)
    case class Config(title: String, host: Host, peers: List[Host])

    given Decoder[TomlValue, Host] = Decoder.derived
    given Decoder[TomlValue, Config] = Decoder.derived

    // Simulate a parsed document: title at root, [host] subtable, [[peers]] array table.
    val hostTable: TomlTable = (
      isArrayTable = false,
      pairs = Map("name" -> TomlValue.String("a"), "port" -> TomlValue.Integer(1)),
      subtables = Map.empty
    )
    val peerA: TomlTable = (
      isArrayTable = true,
      pairs = Map("name" -> TomlValue.String("b"), "port" -> TomlValue.Integer(2)),
      subtables = Map.empty
    )
    val peerB: TomlTable = (
      isArrayTable = true,
      pairs = Map("name" -> TomlValue.String("c"), "port" -> TomlValue.Integer(3)),
      subtables = Map.empty
    )
    val root: TomlTable = (
      isArrayTable = false,
      pairs = Map("title" -> TomlValue.String("cfg")),
      subtables = Map(
        "host" -> List(hostTable),
        "peers" -> List(peerA, peerB)
      )
    )
    assert(!root.isArrayTable)

    Decoder[TomlValue, Config].decode(toInlineValue(root)) match {
      case Result.Success(cfg, _) =>
        assertEquals(cfg.title, "cfg")
        assertEquals(cfg.host, Host("a", 1))
        assertEquals(cfg.peers, List(Host("b", 2), Host("c", 3)))
      case other => fail(s"Expected Success, got $other")
    }
  }
}
