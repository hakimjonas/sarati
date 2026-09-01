package net.ghoula.sarati.ast.json

import munit.FunSuite

class FormatJsonTests extends FunSuite {

  test("format null") {
    assertEquals(formatJson(JsonValue.Null), "null")
  }

  test("format booleans") {
    assertEquals(formatJson(JsonValue.Bool(true)), "true")
    assertEquals(formatJson(JsonValue.Bool(false)), "false")
  }

  test("format whole numbers as integers") {
    assertEquals(formatJson(JsonValue.Number(42.0)), "42")
  }

  test("format fractional numbers") {
    assertEquals(formatJson(JsonValue.Number(3.14)), "3.14")
  }

  test("format strings with escaping") {
    assertEquals(formatJson(JsonValue.Str("hello")), "\"hello\"")
    assertEquals(formatJson(JsonValue.Str("line1\nline2")), "\"line1\\nline2\"")
    assertEquals(formatJson(JsonValue.Str("tab\there")), "\"tab\\there\"")
    assertEquals(formatJson(JsonValue.Str("quote\"inside")), "\"quote\\\"inside\"")
  }

  test("format empty array") {
    assertEquals(formatJson(JsonValue.Array(List.empty)), "[]")
  }

  test("format array compact") {
    val arr = JsonValue.Array(List(JsonValue.Number(1.0), JsonValue.Number(2.0), JsonValue.Number(3.0)))
    assertEquals(formatJson(arr), "[1,2,3]")
  }

  test("format empty object") {
    assertEquals(formatJson(JsonValue.Object(Map.empty)), "{}")
  }

  test("format object compact") {
    val obj = JsonValue.Object(Map("a" -> JsonValue.Number(1.0)))
    assertEquals(formatJson(obj), "{\"a\":1}")
  }

  test("format object pretty") {
    val obj = JsonValue.Object(Map("name" -> JsonValue.Str("test")))
    val result = formatJson(obj, prettyFormat)
    assert(result.contains("\n"), "Pretty format should contain newlines")
    assert(result.contains("  "), "Pretty format should contain indentation")
    assert(result.contains("\"name\""), "Should contain the key")
  }

  test("format with sorted keys") {
    val obj = JsonValue.Object(Map("b" -> JsonValue.Number(2.0), "a" -> JsonValue.Number(1.0)))
    val config: JsonFormatConfig = (indent = 0, newlines = false, sortKeys = true)
    val result = formatJson(obj, config)
    assert(result.indexOf("\"a\"") < result.indexOf("\"b\""), "Keys should be sorted")
  }

  // --- audit fixes: non-finite and large doubles ---

  test("format non-finite doubles as null (JSON has no NaN or infinity)") {
    assertEquals(formatJson(JsonValue.Number(Double.NaN)), "null")
    assertEquals(formatJson(JsonValue.Number(Double.PositiveInfinity)), "null")
    assertEquals(formatJson(JsonValue.Number(Double.NegativeInfinity)), "null")
  }

  test("format whole doubles beyond Long range in exponent form, not saturated longs") {
    assertEquals(formatJson(JsonValue.Number(1e20)), "1.0E20")
    assertEquals(formatJson(JsonValue.Number(-1e20)), "-1.0E20")
    // reparse yields the same double
    assertEquals("1.0E20".toDouble, 1e20)
  }

  test("format whole doubles within Long range as integers") {
    assertEquals(formatJson(JsonValue.Number(9223372036854775807.0)), "9223372036854775807")
    assertEquals(formatJson(JsonValue.Number(-9223372036854775808.0)), "-9223372036854775808")

  }

  // --- raw token (issue #3) ---

  test("Number raw token: None when constructed, preserved when given") {
    assertEquals(JsonValue.Number(42.0).raw, None)
    assertEquals(JsonValue.Number(42.0, Some("42.0")).raw, Some("42.0"))
    assertEquals(JsonValue.Number(1e10, Some("1e10")).value, 1e10)
  }

  test("equality includes the raw token") {
    assert(JsonValue.Number(1.0, Some("1.0")) != JsonValue.Number(1.0))
    assertEquals(JsonValue.Number(1.0, Some("1.0")), JsonValue.Number(1.0, Some("1.0")))
  }
}
