package net.ghoula.sarati.codec

import munit.FunSuite

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime}
import java.util.UUID

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue

class DecoderTests extends FunSuite {

  import JsonDecoders.given

  test("decode JSON string to String") {
    val json = JsonValue.Str("hello")
    val result = Decoder[JsonValue, String].decode(json)
    assertEquals(result, Result.Success("hello", 0))
  }

  test("decode JSON number to Int") {
    val json = JsonValue.Number(42.0)
    val result = Decoder[JsonValue, Int].decode(json)
    assertEquals(result, Result.Success(42, 0))
  }

  test("decode JSON number to Long") {
    val json = JsonValue.Number(123456789.0)
    val result = Decoder[JsonValue, Long].decode(json)
    assertEquals(result, Result.Success(123456789L, 0))
  }

  test("decode JSON number to Double") {
    val json = JsonValue.Number(3.14159)
    val result = Decoder[JsonValue, Double].decode(json)
    assertEquals(result, Result.Success(3.14159, 0))
  }

  test("decode JSON boolean to Boolean") {
    val jsonTrue = JsonValue.Bool(true)
    val jsonFalse = JsonValue.Bool(false)
    assertEquals(Decoder[JsonValue, Boolean].decode(jsonTrue), Result.Success(true, 0))
    assertEquals(Decoder[JsonValue, Boolean].decode(jsonFalse), Result.Success(false, 0))
  }

  test("decode JSON null to Option[A]") {
    val json = JsonValue.Null
    val result = Decoder[JsonValue, Option[String]].decode(json)
    assertEquals(result, Result.Success(None, 0))
  }

  test("type mismatch error - string expected, number found") {
    val json = JsonValue.Number(42.0)
    val result = Decoder[JsonValue, String].decode(json)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("String", actual, _) => actual.contains("Number")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("type mismatch error - int expected, non-whole number found") {
    val json = JsonValue.Number(3.14)
    val result = Decoder[JsonValue, Int].decode(json)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Int", actual, _) => actual.contains("not a whole number")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("decode simple case class with 2 fields") {
    case class Point(x: Int, y: Int)
    given Decoder[JsonValue, Point] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "x" -> JsonValue.Number(10.0),
        "y" -> JsonValue.Number(20.0)
      )
    )

    val result = Decoder[JsonValue, Point].decode(json)
    assertEquals(result, Result.Success(Point(10, 20), 0))
  }

  test("decode case class with 3 fields of mixed types") {
    case class Person(name: String, age: Int, active: Boolean)
    given Decoder[JsonValue, Person] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Alice"),
        "age" -> JsonValue.Number(30.0),
        "active" -> JsonValue.Bool(true)
      )
    )

    val result = Decoder[JsonValue, Person].decode(json)
    assertEquals(result, Result.Success(Person("Alice", 30, true), 0))
  }

  test("decode case class with optional fields") {
    case class User(name: String, email: Option[String])
    given Decoder[JsonValue, User] = Decoder.derived

    val jsonWithEmail = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Bob"),
        "email" -> JsonValue.Str("bob@example.com")
      )
    )

    val jsonWithoutEmail = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Bob"),
        "email" -> JsonValue.Null
      )
    )

    val jsonMissingKey = JsonValue.Object(
      Map("name" -> JsonValue.Str("Bob"))
    )

    val resultWith = Decoder[JsonValue, User].decode(jsonWithEmail)
    val resultWithout = Decoder[JsonValue, User].decode(jsonWithoutEmail)
    val resultMissing = Decoder[JsonValue, User].decode(jsonMissingKey)

    assertEquals(resultWith, Result.Success(User("Bob", Some("bob@example.com")), 0))
    assertEquals(resultWithout, Result.Success(User("Bob", None), 0))
    assertEquals(resultMissing, Result.Success(User("Bob", None), 0))
  }

  test("decode case class with list field") {
    case class Team(name: String, members: List[String])
    given Decoder[JsonValue, Team] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Avengers"),
        "members" -> JsonValue.Array(
          List(
            JsonValue.Str("Iron Man"),
            JsonValue.Str("Captain America"),
            JsonValue.Str("Thor")
          )
        )
      )
    )

    val result = Decoder[JsonValue, Team].decode(json)
    assertEquals(result, Result.Success(Team("Avengers", List("Iron Man", "Captain America", "Thor")), 0))
  }

  test("decode nested case classes") {
    case class Address(street: String, city: String)
    case class Company(name: String, address: Address)

    given Decoder[JsonValue, Address] = Decoder.derived
    given Decoder[JsonValue, Company] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Acme Corp"),
        "address" -> JsonValue.Object(
          Map(
            "street" -> JsonValue.Str("123 Main St"),
            "city" -> JsonValue.Str("Springfield")
          )
        )
      )
    )

    val result = Decoder[JsonValue, Company].decode(json)
    val expected = Company("Acme Corp", Address("123 Main St", "Springfield"))
    assertEquals(result, Result.Success(expected, 0))
  }

  test("missing field error") {
    case class Book(title: String, author: String)
    given Decoder[JsonValue, Book] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "title" -> JsonValue.Str("1984")
      )
    )

    val result = Decoder[JsonValue, Book].decode(json)
    result match {
      case Result.Partial(_, errors, _) =>
        assert(errors.exists {
          case DecodeError.MissingField("author", _) => true
          case _ => false
        })
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.MissingField("author", _) => true
          case _ => false
        })
      case _ => fail("Expected Partial or Failure with missing field error")
    }
  }

  test("invalid field type error") {
    case class Product(name: String, price: Int)
    given Decoder[JsonValue, Product] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Widget"),
        "price" -> JsonValue.Str("not a number")
      )
    )

    val result = Decoder[JsonValue, Product].decode(json)
    result match {
      case Result.Partial(_, errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Int", _, _) => true
          case _ => false
        })
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Int", _, _) => true
          case _ => false
        })
      case _ => fail("Expected Partial or Failure with type mismatch")
    }
  }

  test("decode empty object") {
    case class Empty()
    given Decoder[JsonValue, Empty] = Decoder.derived

    val json = JsonValue.Object(Map.empty)
    val result = Decoder[JsonValue, Empty].decode(json)
    assertEquals(result, Result.Success(Empty(), 0))
  }

  test("decode complex nested structure") {
    case class Tag(name: String)
    case class Post(title: String, content: String, tags: List[Tag])
    case class Author(name: String, posts: List[Post])

    given Decoder[JsonValue, Tag] = Decoder.derived
    given Decoder[JsonValue, Post] = Decoder.derived
    given Decoder[JsonValue, Author] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Alice"),
        "posts" -> JsonValue.Array(
          List(
            JsonValue.Object(
              Map(
                "title" -> JsonValue.Str("Hello World"),
                "content" -> JsonValue.Str("This is my first post"),
                "tags" -> JsonValue.Array(
                  List(
                    JsonValue.Object(Map("name" -> JsonValue.Str("intro"))),
                    JsonValue.Object(Map("name" -> JsonValue.Str("hello")))
                  )
                )
              )
            )
          )
        )
      )
    )

    val result = Decoder[JsonValue, Author].decode(json)
    val expected = Author(
      "Alice",
      List(Post("Hello World", "This is my first post", List(Tag("intro"), Tag("hello"))))
    )
    assertEquals(result, Result.Success(expected, 0))
  }

  test("type mismatch - object expected, array found") {
    case class Item(id: Int)
    given Decoder[JsonValue, Item] = Decoder.derived

    val json = JsonValue.Array(List(JsonValue.Number(1.0)))
    val result = Decoder[JsonValue, Item].decode(json)

    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Object", "Array", _) => true
          case _ => false
        })
      case _ => fail("Expected Failure with type mismatch")
    }
  }

  test("decode JSON array to List[Int]") {
    val json = JsonValue.Array(
      List(
        JsonValue.Number(1.0),
        JsonValue.Number(2.0),
        JsonValue.Number(3.0)
      )
    )

    val result = Decoder[JsonValue, List[Int]].decode(json)
    assertEquals(result, Result.Success(List(1, 2, 3), 0))
  }

  test("decode JSON array to Seq[String]") {
    val json = JsonValue.Array(
      List(
        JsonValue.Str("apple"),
        JsonValue.Str("banana"),
        JsonValue.Str("cherry")
      )
    )

    val result = Decoder[JsonValue, Seq[String]].decode(json)
    assertEquals(result, Result.Success(Seq("apple", "banana", "cherry"), 0))
  }

  test("decode JSON array to Vector[Boolean]") {
    val json = JsonValue.Array(
      List(
        JsonValue.Bool(true),
        JsonValue.Bool(false),
        JsonValue.Bool(true)
      )
    )

    val result = Decoder[JsonValue, Vector[Boolean]].decode(json)
    assertEquals(result, Result.Success(Vector(true, false, true), 0))
  }

  test("decode empty JSON array") {
    val json = JsonValue.Array(List.empty)
    val result = Decoder[JsonValue, List[String]].decode(json)
    assertEquals(result, Result.Success(List.empty, 0))
  }

  test("decode nested arrays") {
    val json = JsonValue.Array(
      List(
        JsonValue.Array(List(JsonValue.Number(1.0), JsonValue.Number(2.0))),
        JsonValue.Array(List(JsonValue.Number(3.0), JsonValue.Number(4.0)))
      )
    )

    val result = Decoder[JsonValue, List[List[Int]]].decode(json)
    assertEquals(result, Result.Success(List(List(1, 2), List(3, 4)), 0))
  }

  test("decode JSON object to Map[String, Int]") {
    val json = JsonValue.Object(
      Map(
        "a" -> JsonValue.Number(1.0),
        "b" -> JsonValue.Number(2.0),
        "c" -> JsonValue.Number(3.0)
      )
    )

    val result = Decoder[JsonValue, Map[String, Int]].decode(json)
    assertEquals(result, Result.Success(Map("a" -> 1, "b" -> 2, "c" -> 3), 0))
  }

  test("decode case class with all primitive types") {
    case class AllTypes(
      s: String,
      i: Int,
      l: Long,
      d: Double,
      b: Boolean
    )
    given Decoder[JsonValue, AllTypes] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "s" -> JsonValue.Str("test"),
        "i" -> JsonValue.Number(42.0),
        "l" -> JsonValue.Number(1000000.0),
        "d" -> JsonValue.Number(3.14),
        "b" -> JsonValue.Bool(true)
      )
    )

    val result = Decoder[JsonValue, AllTypes].decode(json)
    assertEquals(result, Result.Success(AllTypes("test", 42, 1000000L, 3.14, true), 0))
  }

  test("decode Option[List[Int]]") {
    val jsonSome = JsonValue.Array(
      List(
        JsonValue.Number(1.0),
        JsonValue.Number(2.0)
      )
    )
    val jsonNone = JsonValue.Null

    val resultSome = Decoder[JsonValue, Option[List[Int]]].decode(jsonSome)
    val resultNone = Decoder[JsonValue, Option[List[Int]]].decode(jsonNone)

    assertEquals(resultSome, Result.Success(Some(List(1, 2)), 0))
    assertEquals(resultNone, Result.Success(None, 0))
  }

  test("decode List[Option[String]]") {
    val json = JsonValue.Array(
      List(
        JsonValue.Str("hello"),
        JsonValue.Null,
        JsonValue.Str("world")
      )
    )

    val result = Decoder[JsonValue, List[Option[String]]].decode(json)
    assertEquals(result, Result.Success(List(Some("hello"), None, Some("world")), 0))
  }

  test("decode case class with Byte, Short, and Float") {
    case class SmallNumbers(b: Byte, s: Short, f: Float)
    given Decoder[JsonValue, SmallNumbers] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "b" -> JsonValue.Number(127.0),
        "s" -> JsonValue.Number(32000.0),
        "f" -> JsonValue.Number(1.5)
      )
    )

    val result = Decoder[JsonValue, SmallNumbers].decode(json)
    assertEquals(result, Result.Success(SmallNumbers(127.toByte, 32000.toShort, 1.5f), 0))
  }

  test("decode case class with BigInt and BigDecimal") {
    case class BigNumbers(bi: BigInt, bd: BigDecimal)
    given Decoder[JsonValue, BigNumbers] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "bi" -> JsonValue.Number(123456789.0),
        "bd" -> JsonValue.Number(123.456)
      )
    )

    val result = Decoder[JsonValue, BigNumbers].decode(json)
    assertEquals(result, Result.Success(BigNumbers(BigInt(123456789), BigDecimal(123.456)), 0))
  }

  test("decode Instant from ISO-8601 string") {
    val json = JsonValue.Str("2024-01-15T10:30:00Z")
    val result = Decoder[JsonValue, Instant].decode(json)
    assertEquals(result, Result.Success(Instant.parse("2024-01-15T10:30:00Z"), 0))
  }

  test("decode Instant - invalid format") {
    val json = JsonValue.Str("not-a-date")
    val result = Decoder[JsonValue, Instant].decode(json)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch(exp, _, _) => exp.contains("Instant")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("decode Instant - wrong type") {
    val json = JsonValue.Number(12345.0)
    val result = Decoder[JsonValue, Instant].decode(json)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("Instant", _, _) => true
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("decode LocalDate from ISO-8601 string") {
    val json = JsonValue.Str("2024-01-15")
    val result = Decoder[JsonValue, LocalDate].decode(json)
    assertEquals(result, Result.Success(LocalDate.of(2024, 1, 15), 0))
  }

  test("decode LocalDate - invalid format") {
    val json = JsonValue.Str("01-15-2024")
    val result = Decoder[JsonValue, LocalDate].decode(json)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch(exp, _, _) => exp.contains("LocalDate")
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("decode LocalDateTime from ISO-8601 string") {
    val json = JsonValue.Str("2024-01-15T10:30:00")
    val result = Decoder[JsonValue, LocalDateTime].decode(json)
    assertEquals(result, Result.Success(LocalDateTime.of(2024, 1, 15, 10, 30, 0), 0))
  }

  test("decode LocalTime from ISO-8601 string") {
    val json = JsonValue.Str("10:30:00")
    val result = Decoder[JsonValue, LocalTime].decode(json)
    assertEquals(result, Result.Success(LocalTime.of(10, 30, 0), 0))
  }

  test("decode LocalTime - with nanoseconds") {
    val json = JsonValue.Str("10:30:00.123456789")
    val result = Decoder[JsonValue, LocalTime].decode(json)
    assertEquals(result, Result.Success(LocalTime.of(10, 30, 0, 123456789), 0))
  }

  test("decode OffsetDateTime from ISO-8601 string") {
    val json = JsonValue.Str("2024-01-15T10:30:00+01:00")
    val result = Decoder[JsonValue, OffsetDateTime].decode(json)
    assertEquals(result, Result.Success(OffsetDateTime.parse("2024-01-15T10:30:00+01:00"), 0))
  }

  test("decode ZonedDateTime from ISO-8601 string") {
    val json = JsonValue.Str("2024-01-15T10:30:00+01:00[Europe/Paris]")
    val result = Decoder[JsonValue, ZonedDateTime].decode(json)
    assertEquals(result, Result.Success(ZonedDateTime.parse("2024-01-15T10:30:00+01:00[Europe/Paris]"), 0))
  }

  test("decode UUID from string") {
    val uuid = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
    val json = JsonValue.Str("550e8400-e29b-41d4-a716-446655440000")
    val result = Decoder[JsonValue, UUID].decode(json)
    assertEquals(result, Result.Success(uuid, 0))
  }

  test("decode UUID - invalid format") {
    val json = JsonValue.Str("not-a-uuid")
    val result = Decoder[JsonValue, UUID].decode(json)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("UUID", _, _) => true
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("decode UUID - wrong type") {
    val json = JsonValue.Number(12345.0)
    val result = Decoder[JsonValue, UUID].decode(json)
    result match {
      case Result.Failure(errors, _) =>
        assert(errors.exists {
          case DecodeError.TypeMismatch("UUID", _, _) => true
          case _ => false
        })
      case _ => fail("Expected Failure")
    }
  }

  test("decode case class with Instant field") {
    case class Event(name: String, timestamp: Instant)
    given Decoder[JsonValue, Event] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Meeting"),
        "timestamp" -> JsonValue.Str("2024-01-15T10:30:00Z")
      )
    )

    val result = Decoder[JsonValue, Event].decode(json)
    val expected = Event("Meeting", Instant.parse("2024-01-15T10:30:00Z"))
    assertEquals(result, Result.Success(expected, 0))
  }

  test("decode case class with LocalDate field") {
    case class Birthday(name: String, date: LocalDate)
    given Decoder[JsonValue, Birthday] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Alice"),
        "date" -> JsonValue.Str("1990-05-20")
      )
    )

    val result = Decoder[JsonValue, Birthday].decode(json)
    val expected = Birthday("Alice", LocalDate.of(1990, 5, 20))
    assertEquals(result, Result.Success(expected, 0))
  }

  test("decode case class with UUID field") {
    case class Entity(id: UUID, name: String)
    given Decoder[JsonValue, Entity] = Decoder.derived

    val uuid = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
    val json = JsonValue.Object(
      Map(
        "id" -> JsonValue.Str("550e8400-e29b-41d4-a716-446655440000"),
        "name" -> JsonValue.Str("Test Entity")
      )
    )

    val result = Decoder[JsonValue, Entity].decode(json)
    val expected = Entity(uuid, "Test Entity")
    assertEquals(result, Result.Success(expected, 0))
  }

  test("decode case class with optional Instant") {
    case class Log(message: String, timestamp: Option[Instant])
    given Decoder[JsonValue, Log] = Decoder.derived

    val jsonWithTimestamp = JsonValue.Object(
      Map(
        "message" -> JsonValue.Str("Hello"),
        "timestamp" -> JsonValue.Str("2024-01-15T10:30:00Z")
      )
    )

    val jsonWithoutTimestamp = JsonValue.Object(
      Map(
        "message" -> JsonValue.Str("Hello"),
        "timestamp" -> JsonValue.Null
      )
    )

    val resultWith = Decoder[JsonValue, Log].decode(jsonWithTimestamp)
    val resultWithout = Decoder[JsonValue, Log].decode(jsonWithoutTimestamp)

    assertEquals(resultWith, Result.Success(Log("Hello", Some(Instant.parse("2024-01-15T10:30:00Z"))), 0))
    assertEquals(resultWithout, Result.Success(Log("Hello", None), 0))
  }

  test("decode case class with List of UUIDs") {
    case class Team(name: String, memberIds: List[UUID])
    given Decoder[JsonValue, Team] = Decoder.derived

    val uuid1 = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
    val uuid2 = UUID.fromString("660e8400-e29b-41d4-a716-446655440001")

    val json = JsonValue.Object(
      Map(
        "name" -> JsonValue.Str("Alpha Team"),
        "memberIds" -> JsonValue.Array(
          List(
            JsonValue.Str("550e8400-e29b-41d4-a716-446655440000"),
            JsonValue.Str("660e8400-e29b-41d4-a716-446655440001")
          )
        )
      )
    )

    val result = Decoder[JsonValue, Team].decode(json)
    val expected = Team("Alpha Team", List(uuid1, uuid2))
    assertEquals(result, Result.Success(expected, 0))
  }

  // --- audit fixes: BigInt decoder ---

  test("decode BigInt exactly for whole doubles beyond Long range") {
    // 1e19 is exactly representable as a Double and beyond Long.MaxValue
    assertEquals(
      Decoder[JsonValue, BigInt].decode(JsonValue.Number(1e19)),
      Result.Success(BigInt("10000000000000000000"), 0)
    )
    assertEquals(
      Decoder[JsonValue, BigInt].decode(JsonValue.Number(9.5e18)),
      Result.Success(BigInt("9500000000000000000"), 0)
    )
  }

  test("decode BigInt fails for non-whole and non-number values") {
    Decoder[JsonValue, BigInt].decode(JsonValue.Number(1.5)) match {
      case Result.Failure(DecodeError.TypeMismatch("BigInt", _, _) :: Nil, _) => ()
      case other => fail(s"expected TypeMismatch failure, got $other")
    }
    Decoder[JsonValue, BigInt].decode(JsonValue.Str("12")) match {
      case Result.Failure(_, _) => ()
      case other => fail(s"expected failure, got $other")
    }
  }
}
