package net.ghoula.sarati.codec

import munit.FunSuite

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime}
import java.util.UUID

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue

class EncoderTests extends FunSuite {

  import JsonDecoders.given
  import JsonEncoders.given

  test("encode String") {
    val result = Encoder[String, JsonValue].encode("hello")
    assertEquals(result, JsonValue.Str("hello"))
  }

  test("encode Int") {
    val result = Encoder[Int, JsonValue].encode(42)
    assertEquals(result, JsonValue.Number(42.0))
  }

  test("encode Long") {
    val result = Encoder[Long, JsonValue].encode(123456789L)
    assertEquals(result, JsonValue.Number(123456789.0))
  }

  test("encode Double") {
    val result = Encoder[Double, JsonValue].encode(3.14)
    assertEquals(result, JsonValue.Number(3.14))
  }

  test("encode Boolean") {
    assertEquals(Encoder[Boolean, JsonValue].encode(true), JsonValue.Bool(true))
    assertEquals(Encoder[Boolean, JsonValue].encode(false), JsonValue.Bool(false))
  }

  test("encode Byte") {
    val result = Encoder[Byte, JsonValue].encode(127.toByte)
    assertEquals(result, JsonValue.Number(127.0))
  }

  test("encode Short") {
    val result = Encoder[Short, JsonValue].encode(32000.toShort)
    assertEquals(result, JsonValue.Number(32000.0))
  }

  test("encode Float") {
    val result = Encoder[Float, JsonValue].encode(1.5f)
    assertEquals(result, JsonValue.Number(1.5))
  }

  test("encode BigInt") {
    val result = Encoder[BigInt, JsonValue].encode(BigInt(123456789))
    assertEquals(result, JsonValue.Number(123456789.0))
  }

  test("encode BigDecimal") {
    val result = Encoder[BigDecimal, JsonValue].encode(BigDecimal(123.456))
    assertEquals(result, JsonValue.Number(123.456))
  }

  test("encode Instant") {
    val instant = Instant.parse("2024-01-15T10:30:00Z")
    val result = Encoder[Instant, JsonValue].encode(instant)
    assertEquals(result, JsonValue.Str("2024-01-15T10:30:00Z"))
  }

  test("encode LocalDate") {
    val date = LocalDate.of(2024, 1, 15)
    val result = Encoder[LocalDate, JsonValue].encode(date)
    assertEquals(result, JsonValue.Str("2024-01-15"))
  }

  test("encode LocalDateTime") {
    val dt = LocalDateTime.of(2024, 1, 15, 10, 30, 0)
    val result = Encoder[LocalDateTime, JsonValue].encode(dt)
    assertEquals(result, JsonValue.Str("2024-01-15T10:30"))
  }

  test("encode LocalTime") {
    val time = LocalTime.of(10, 30, 0)
    val result = Encoder[LocalTime, JsonValue].encode(time)
    assertEquals(result, JsonValue.Str("10:30"))
  }

  test("encode UUID") {
    val uuid = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
    val result = Encoder[UUID, JsonValue].encode(uuid)
    assertEquals(result, JsonValue.Str("550e8400-e29b-41d4-a716-446655440000"))
  }

  test("encode OffsetDateTime") {
    val odt = OffsetDateTime.parse("2024-01-15T10:30:00+01:00")
    val result = Encoder[OffsetDateTime, JsonValue].encode(odt)
    assertEquals(result, JsonValue.Str("2024-01-15T10:30+01:00"))
  }

  test("encode ZonedDateTime") {
    val zdt = ZonedDateTime.parse("2024-01-15T10:30:00+01:00[Europe/Paris]")
    val result = Encoder[ZonedDateTime, JsonValue].encode(zdt)
    assertEquals(result, JsonValue.Str("2024-01-15T10:30+01:00[Europe/Paris]"))
  }

  test("encode None => Null") {
    val result = Encoder[Option[String], JsonValue].encode(None)
    assertEquals(result, JsonValue.Null)
  }

  test("encode Some(value)") {
    val result = Encoder[Option[String], JsonValue].encode(Some("hello"))
    assertEquals(result, JsonValue.Str("hello"))
  }

  test("encode Some(42)") {
    val result = Encoder[Option[Int], JsonValue].encode(Some(42))
    assertEquals(result, JsonValue.Number(42.0))
  }

  test("encode List[Int]") {
    val result = Encoder[List[Int], JsonValue].encode(List(1, 2, 3))
    assertEquals(result, JsonValue.Array(List(JsonValue.Number(1.0), JsonValue.Number(2.0), JsonValue.Number(3.0))))
  }

  test("encode empty List") {
    val result = Encoder[List[Int], JsonValue].encode(List.empty)
    assertEquals(result, JsonValue.Array(List.empty))
  }

  test("encode Seq[String]") {
    val result = Encoder[Seq[String], JsonValue].encode(Seq("a", "b"))
    assertEquals(result, JsonValue.Array(List(JsonValue.Str("a"), JsonValue.Str("b"))))
  }

  test("encode Vector[Boolean]") {
    val result = Encoder[Vector[Boolean], JsonValue].encode(Vector(true, false))
    assertEquals(result, JsonValue.Array(List(JsonValue.Bool(true), JsonValue.Bool(false))))
  }

  test("encode Map[String, Int]") {
    val result = Encoder[Map[String, Int], JsonValue].encode(Map("a" -> 1, "b" -> 2))
    assertEquals(result, JsonValue.Object(Map("a" -> JsonValue.Number(1.0), "b" -> JsonValue.Number(2.0))))
  }

  test("encode simple 2-field case class") {
    case class Point(x: Int, y: Int)
    given Encoder[Point, JsonValue] = Encoder.derived

    val result = Encoder[Point, JsonValue].encode(Point(10, 20))
    assertEquals(result, JsonValue.Object(Map("x" -> JsonValue.Number(10.0), "y" -> JsonValue.Number(20.0))))
  }

  test("encode mixed 3-field case class") {
    case class Person(name: String, age: Int, active: Boolean)
    given Encoder[Person, JsonValue] = Encoder.derived

    val result = Encoder[Person, JsonValue].encode(Person("Alice", 30, true))
    assertEquals(
      result,
      JsonValue.Object(
        Map(
          "name" -> JsonValue.Str("Alice"),
          "age" -> JsonValue.Number(30.0),
          "active" -> JsonValue.Bool(true)
        )
      )
    )
  }

  test("encode case class with optional Some") {
    case class User(name: String, email: Option[String])
    given Encoder[User, JsonValue] = Encoder.derived

    val result = Encoder[User, JsonValue].encode(User("Bob", Some("bob@example.com")))
    assertEquals(
      result,
      JsonValue.Object(
        Map(
          "name" -> JsonValue.Str("Bob"),
          "email" -> JsonValue.Str("bob@example.com")
        )
      )
    )
  }

  test("encode case class with optional None") {
    case class User(name: String, email: Option[String])
    given Encoder[User, JsonValue] = Encoder.derived

    val result = Encoder[User, JsonValue].encode(User("Bob", None))
    assertEquals(
      result,
      JsonValue.Object(
        Map(
          "name" -> JsonValue.Str("Bob"),
          "email" -> JsonValue.Null
        )
      )
    )
  }

  test("encode case class with list field") {
    case class Team(name: String, members: List[String])
    given Encoder[Team, JsonValue] = Encoder.derived

    val result = Encoder[Team, JsonValue].encode(Team("Avengers", List("Iron Man", "Thor")))
    assertEquals(
      result,
      JsonValue.Object(
        Map(
          "name" -> JsonValue.Str("Avengers"),
          "members" -> JsonValue.Array(List(JsonValue.Str("Iron Man"), JsonValue.Str("Thor")))
        )
      )
    )
  }

  test("encode nested case classes") {
    case class Address(street: String, city: String)
    case class Company(name: String, address: Address)

    given Encoder[Address, JsonValue] = Encoder.derived
    given Encoder[Company, JsonValue] = Encoder.derived

    val result =
      Encoder[Company, JsonValue].encode(Company("Acme Corp", Address("123 Main St", "Springfield")))
    assertEquals(
      result,
      JsonValue.Object(
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
    )
  }

  test("encode empty case class") {
    case class Empty()
    given Encoder[Empty, JsonValue] = Encoder.derived

    val result = Encoder[Empty, JsonValue].encode(Empty())
    assertEquals(result, JsonValue.Object(Map.empty))
  }

  test("round-trip simple case class") {
    case class Point(x: Int, y: Int)
    given Encoder[Point, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Point] = Decoder.derived

    val original = Point(10, 20)
    val encoded = Encoder[Point, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, Point].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip optional fields") {
    case class User(name: String, email: Option[String])
    given Encoder[User, JsonValue] = Encoder.derived
    given Decoder[JsonValue, User] = Decoder.derived

    val withEmail = User("Bob", Some("bob@example.com"))
    val noEmail = User("Bob", None)

    assertEquals(
      Decoder[JsonValue, User].decode(Encoder[User, JsonValue].encode(withEmail)),
      Result.Success(withEmail, 0)
    )
    assertEquals(Decoder[JsonValue, User].decode(Encoder[User, JsonValue].encode(noEmail)), Result.Success(noEmail, 0))
  }

  test("round-trip list field") {
    case class Team(name: String, members: List[String])
    given Encoder[Team, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Team] = Decoder.derived

    val original = Team("Avengers", List("Iron Man", "Thor"))
    val encoded = Encoder[Team, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, Team].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip nested case classes") {
    case class Address(street: String, city: String)
    case class Company(name: String, address: Address)

    given Encoder[Address, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Address] = Decoder.derived
    given Encoder[Company, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Company] = Decoder.derived

    val original = Company("Acme Corp", Address("123 Main St", "Springfield"))
    val encoded = Encoder[Company, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, Company].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip all primitives") {
    case class AllTypes(s: String, i: Int, l: Long, d: Double, b: Boolean)
    given Encoder[AllTypes, JsonValue] = Encoder.derived
    given Decoder[JsonValue, AllTypes] = Decoder.derived

    val original = AllTypes("test", 42, 1000000L, 3.14, true)
    val encoded = Encoder[AllTypes, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, AllTypes].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip Instant") {
    val original = Instant.parse("2024-01-15T10:30:00Z")
    val encoded = Encoder[Instant, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, Instant].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip UUID") {
    val original = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
    val encoded = Encoder[UUID, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, UUID].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip complex structure") {
    case class Tag(name: String)
    case class Post(title: String, content: String, tags: List[Tag])
    case class Author(name: String, posts: List[Post])

    given Encoder[Tag, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Tag] = Decoder.derived
    given Encoder[Post, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Post] = Decoder.derived
    given Encoder[Author, JsonValue] = Encoder.derived
    given Decoder[JsonValue, Author] = Decoder.derived

    val original = Author(
      "Alice",
      List(Post("Hello World", "This is my first post", List(Tag("intro"), Tag("hello"))))
    )
    val encoded = Encoder[Author, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, Author].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip Byte, Short, Float") {
    case class SmallNumbers(b: Byte, s: Short, f: Float)
    given Encoder[SmallNumbers, JsonValue] = Encoder.derived
    given Decoder[JsonValue, SmallNumbers] = Decoder.derived

    val original = SmallNumbers(127.toByte, 32000.toShort, 1.5f)
    val encoded = Encoder[SmallNumbers, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, SmallNumbers].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip BigInt and BigDecimal") {
    case class BigNumbers(bi: BigInt, bd: BigDecimal)
    given Encoder[BigNumbers, JsonValue] = Encoder.derived
    given Decoder[JsonValue, BigNumbers] = Decoder.derived

    val original = BigNumbers(BigInt(123456789), BigDecimal(123.456))
    val encoded = Encoder[BigNumbers, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, BigNumbers].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip LocalDate") {
    val original = LocalDate.of(2024, 1, 15)
    val encoded = Encoder[LocalDate, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, LocalDate].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip LocalDateTime") {
    val original = LocalDateTime.of(2024, 1, 15, 10, 30, 0)
    val encoded = Encoder[LocalDateTime, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, LocalDateTime].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip LocalTime") {
    val original = LocalTime.of(10, 30, 0)
    val encoded = Encoder[LocalTime, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, LocalTime].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip OffsetDateTime") {
    val original = OffsetDateTime.parse("2024-01-15T10:30:00+01:00")
    val encoded = Encoder[OffsetDateTime, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, OffsetDateTime].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("round-trip ZonedDateTime") {
    val original = ZonedDateTime.parse("2024-01-15T10:30:00+01:00[Europe/Paris]")
    val encoded = Encoder[ZonedDateTime, JsonValue].encode(original)
    val decoded = Decoder[JsonValue, ZonedDateTime].decode(encoded)
    assertEquals(decoded, Result.Success(original, 0))
  }

  test("contramap derives encoder from existing encoder") {
    case class UserId(value: String)
    val userIdEncoder: Encoder[UserId, JsonValue] =
      Encoder[String, JsonValue].contramap[UserId](_.value)

    assertEquals(userIdEncoder.encode(UserId("abc-123")), JsonValue.Str("abc-123"))
  }

  test("contramap composes with derived encoder") {
    case class Point(x: Int, y: Int)
    given Encoder[Point, JsonValue] = Encoder.derived

    case class LabeledPoint(label: String, point: Point)
    val encoder: Encoder[LabeledPoint, JsonValue] =
      Encoder[Point, JsonValue].contramap[LabeledPoint](_.point)

    val result = encoder.encode(LabeledPoint("origin", Point(0, 0)))
    assertEquals(result, JsonValue.Object(Map("x" -> JsonValue.Number(0.0), "y" -> JsonValue.Number(0.0))))
  }

  test("Long precision loss beyond 2^53 is a known limitation") {
    val beyondPrecision = 9007199254740993L
    val encoded = Encoder[Long, JsonValue].encode(beyondPrecision)

    val JsonValue.Number(n, _) = encoded: @unchecked
    assertNotEquals(n.toLong, beyondPrecision)
  }
}
