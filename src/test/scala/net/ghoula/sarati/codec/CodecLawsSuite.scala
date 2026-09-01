package net.ghoula.sarati.codec

import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen, Prop}

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue

/** Property laws for the derived codec.
  *
  * The round-trip law is the correctness oracle for the traversal refactors: encode then decode
  * must reproduce the input exactly, regardless of how the codec is implemented internally. The
  * failure laws pin the error-channel contract.
  */
class CodecLawsSuite extends ScalaCheckSuite {

  import JsonDecoders.given
  import JsonEncoders.given

  private case class Point(x: Int, y: Int)
  private case class Person(name: String, age: Int, active: Boolean)
  private case class Address(street: String, city: String, zip: String)
  private case class User(name: String, email: String, address: Address)

  private given Decoder[JsonValue, Point] = Decoder.derived
  private given Decoder[JsonValue, Person] = Decoder.derived
  private given Decoder[JsonValue, Address] = Decoder.derived
  private given Decoder[JsonValue, User] = Decoder.derived

  private given Encoder[Point, JsonValue] = Encoder.derived
  private given Encoder[Person, JsonValue] = Encoder.derived
  private given Encoder[Address, JsonValue] = Encoder.derived
  private given Encoder[User, JsonValue] = Encoder.derived

  private given Arbitrary[Point] = Arbitrary {
    for
      x <- Gen.choose(-100000, 100000)
      y <- Gen.choose(-100000, 100000)
    yield Point(x, y)
  }

  private given Arbitrary[Address] = Arbitrary {
    for
      street <- Gen.alphaNumStr
      city <- Gen.alphaNumStr
      zip <- Gen.alphaNumStr
    yield Address(street, city, zip)
  }

  private given Arbitrary[Person] = Arbitrary {
    for
      name <- Gen.alphaNumStr
      age <- Gen.choose(0, 120)
      active <- Gen.oneOf(true, false)
    yield Person(name, age, active)
  }

  private given Arbitrary[User] = Arbitrary {
    for
      name <- Gen.alphaNumStr
      email <- Gen.alphaNumStr
      address <- Arbitrary.arbitrary[Address]
    yield User(name, email, address)
  }

  property("round-trip: Point") {
    Prop.forAll { (p: Point) =>
      Decoder[JsonValue, Point].decode(Encoder[Point, JsonValue].encode(p)) match {
        case Result.Success(decoded, 0) => decoded.equals(p)
        case _ => false
      }
    }
  }

  property("round-trip: Person") {
    Prop.forAll { (p: Person) =>
      Decoder[JsonValue, Person].decode(Encoder[Person, JsonValue].encode(p)) match {
        case Result.Success(decoded, 0) => decoded.equals(p)
        case _ => false
      }
    }
  }

  property("round-trip: nested User") {
    Prop.forAll { (u: User) =>
      Decoder[JsonValue, User].decode(Encoder[User, JsonValue].encode(u)) match {
        case Result.Success(decoded, 0) => decoded.equals(u)
        case _ => false
      }
    }
  }

  property("round-trip: List[Int]") {
    Prop.forAll { (xs: List[Int]) =>
      Decoder[JsonValue, List[Int]].decode(Encoder[List[Int], JsonValue].encode(xs)) match {
        case Result.Success(decoded, 0) => decoded.equals(xs)
        case _ => false
      }
    }
  }

  property("round-trip: Map[String, Int]") {
    Prop.forAll { (m: Map[String, Int]) =>
      Decoder[JsonValue, Map[String, Int]].decode(Encoder[Map[String, Int], JsonValue].encode(m)) match {
        case Result.Success(decoded, 0) => decoded.equals(m)
        case _ => false
      }
    }
  }

  property("decode of a mismatched shape is a TypeMismatch failure") {
    Prop.forAll { (s: String) =>
      Decoder[JsonValue, Point].decode(JsonValue.Str(s)) match {
        case Result.Failure(errors, _) =>
          errors.exists {
            case DecodeError.TypeMismatch("Object", _, _) => true
            case _ => false
          }
        case _ => false
      }
    }
  }

  property("decode of an object missing a field is a MissingField failure") {
    Prop.forAll { (x: Int) =>
      Decoder[JsonValue, Point].decode(JsonValue.Object(Map("x" -> JsonValue.Number(x.toDouble)))) match {
        case Result.Failure(errors, _) =>
          errors.exists {
            case DecodeError.MissingField("y", _) => true
            case _ => false
          }
        case _ => false
      }
    }
  }
}
