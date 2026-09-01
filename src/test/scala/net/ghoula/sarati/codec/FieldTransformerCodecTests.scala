package net.ghoula.sarati.codec

import munit.FunSuite

import net.ghoula.sarati.*
import net.ghoula.sarati.ast.json.JsonValue

class FieldTransformerCodecTests extends FunSuite {

  import JsonDecoders.given
  import JsonEncoders.given

  case class UserProfile(firstName: String, lastName: String, emailAddress: String)

  case class WithOptionalSecret(account: Int, note: Option[String])
  case class WithRequiredSecret(account: Int, secret: String)

  /** Excludes exactly one field by its case-class name; renames nothing. */
  object ExcludeNote extends FieldTransformer {
    def transformFieldName(fieldName: String): String = fieldName
    def shouldIncludeField(fieldName: String): Boolean = fieldName != "note"
  }

  test("Encoder with SnakeCase transforms field names") {
    given FieldTransformer = FieldTransformers.SnakeCase
    given Encoder[UserProfile, JsonValue] = Encoder.derived

    val encoded = Encoder[UserProfile, JsonValue].encode(UserProfile("John", "Doe", "john@example.com"))
    encoded match {
      case JsonValue.Object(fields) =>
        assert(fields.contains("first_name"), s"Expected 'first_name', got keys: ${fields.keys}")
        assert(fields.contains("last_name"), s"Expected 'last_name', got keys: ${fields.keys}")
        assert(fields.contains("email_address"), s"Expected 'email_address', got keys: ${fields.keys}")
        assertEquals(fields("first_name"), JsonValue.Str("John"))
      case other => fail(s"Expected Object, got $other")
    }
  }

  test("Decoder with SnakeCase reads transformed field names") {
    given FieldTransformer = FieldTransformers.SnakeCase
    given Decoder[JsonValue, UserProfile] = Decoder.derived

    val json = JsonValue.Object(
      Map(
        "first_name" -> JsonValue.Str("Jane"),
        "last_name" -> JsonValue.Str("Smith"),
        "email_address" -> JsonValue.Str("jane@example.com")
      )
    )

    Decoder[JsonValue, UserProfile].decode(json) match {
      case Result.Success(profile, _) =>
        assertEquals(profile.firstName, "Jane")
        assertEquals(profile.lastName, "Smith")
        assertEquals(profile.emailAddress, "jane@example.com")
      case other => fail(s"Expected Success, got $other")
    }
  }

  test("Encoder/Decoder round-trip with SnakeCase") {
    given FieldTransformer = FieldTransformers.SnakeCase
    given Encoder[UserProfile, JsonValue] = Encoder.derived
    given Decoder[JsonValue, UserProfile] = Decoder.derived

    val original = UserProfile("Alice", "Wonder", "alice@example.com")
    val encoded = Encoder[UserProfile, JsonValue].encode(original)
    Decoder[JsonValue, UserProfile].decode(encoded) match {
      case Result.Success(decoded, _) => assertEquals(decoded, original)
      case other => fail(s"Expected Success, got $other")
    }
  }

  test("Encoder with KebabCase transforms field names") {
    given FieldTransformer = FieldTransformers.KebabCase
    given Encoder[UserProfile, JsonValue] = Encoder.derived

    val encoded = Encoder[UserProfile, JsonValue].encode(UserProfile("John", "Doe", "j@x.com"))
    encoded match {
      case JsonValue.Object(fields) =>
        assert(fields.contains("first-name"), s"Expected 'first-name', got keys: ${fields.keys}")
        assert(fields.contains("last-name"), s"Expected 'last-name', got keys: ${fields.keys}")
        assert(fields.contains("email-address"), s"Expected 'email-address', got keys: ${fields.keys}")
      case other => fail(s"Expected Object, got $other")
    }
  }

  test("Default FieldTransformer preserves field names") {
    // No explicit FieldTransformer given — uses defaultFieldTransformer (identity)
    given Encoder[UserProfile, JsonValue] = Encoder.derived

    val encoded = Encoder[UserProfile, JsonValue].encode(UserProfile("John", "Doe", "j@x.com"))
    encoded match {
      case JsonValue.Object(fields) =>
        assert(fields.contains("firstName"), s"Expected 'firstName', got keys: ${fields.keys}")
        assert(fields.contains("lastName"), s"Expected 'lastName', got keys: ${fields.keys}")
        assert(fields.contains("emailAddress"), s"Expected 'emailAddress', got keys: ${fields.keys}")
      case other => fail(s"Expected Object, got $other")
    }
  }

  test("Encoder with shouldIncludeField=false omits the key") {
    given FieldTransformer = ExcludeNote
    given Encoder[WithOptionalSecret, JsonValue] = Encoder.derived

    Encoder[WithOptionalSecret, JsonValue].encode(WithOptionalSecret(7, Some("hi"))) match {
      case JsonValue.Object(fields) =>
        assert(fields.contains("account"), s"keys: ${fields.keys}")
        assert(!fields.contains("note"), s"excluded key must be absent: ${fields.keys}")
      case other => fail(s"Expected Object, got $other")
    }
  }

  test("Decoder treats an excluded optional field as absent") {
    given FieldTransformer = ExcludeNote
    given Decoder[JsonValue, WithOptionalSecret] = Decoder.derived

    val json = JsonValue.Object(Map("account" -> JsonValue.Number(9)))
    Decoder[JsonValue, WithOptionalSecret].decode(json) match {
      case Result.Success(v, _) =>
        assertEquals(v.account, 9)
        assertEquals(v.note, None)
      case other => fail(s"Expected Success, got $other")
    }
  }

  test("Decoder treats an excluded required field as missing") {
    given FieldTransformer = ExcludeNote
    given Decoder[JsonValue, WithOptionalSecret] = Decoder.derived

    val json = JsonValue.Object(Map("account" -> JsonValue.Number(9), "note" -> JsonValue.Str("x")))
    Decoder[JsonValue, WithOptionalSecret].decode(json) match {
      case Result.Success(v, _) =>
        // excluded even when the key is present in the input
        assertEquals(v.note, None)
      case other => fail(s"Expected Success, got $other")
    }

    given Decoder[JsonValue, WithRequiredSecret] = Decoder.derived
    val result = Decoder[JsonValue, WithRequiredSecret].decode(JsonValue.Object(Map("account" -> JsonValue.Number(1))))
    assert(result.isFailure, s"required excluded field must fail: $result")
    assert(
      result.errors.collectFirst { case DecodeError.MissingField(name, _) => name }.contains("secret"),
      s"errors: ${result.errors}"
    )
  }

  test("Excluded fields are excluded on round-trip (no key echo)") {
    given FieldTransformer = ExcludeNote
    given Encoder[WithOptionalSecret, JsonValue] = Encoder.derived

    val encoded = Encoder[WithOptionalSecret, JsonValue].encode(WithOptionalSecret(1, None))
    encoded match {
      case JsonValue.Object(fields) => assertEquals(fields.keys.toList, List("account"))
      case other => fail(s"Expected Object, got $other")
    }
  }
}
