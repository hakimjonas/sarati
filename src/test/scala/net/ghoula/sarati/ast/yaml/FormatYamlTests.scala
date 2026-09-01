package net.ghoula.sarati.ast.yaml

import munit.FunSuite

class FormatYamlTests extends FunSuite {

  test("format null") {
    assertEquals(formatYaml(YamlValue.Null), "null")
  }

  test("format booleans") {
    assertEquals(formatYaml(YamlValue.Boolean(true)), "true")
    assertEquals(formatYaml(YamlValue.Boolean(false)), "false")
  }

  test("format integer") {
    assertEquals(formatYaml(YamlValue.Integer(42L)), "42")
  }

  test("format float") {
    assertEquals(formatYaml(YamlValue.Float(3.14)), "3.14")
  }

  test("format float special values") {
    assertEquals(formatYaml(YamlValue.Float(Double.NaN)), ".nan")
    assertEquals(formatYaml(YamlValue.Float(Double.PositiveInfinity)), ".inf")
    assertEquals(formatYaml(YamlValue.Float(Double.NegativeInfinity)), "-.inf")
  }

  test("format plain string") {
    assertEquals(formatYaml(YamlValue.String("hello")), "hello")
  }

  test("format string that needs quoting") {
    assertEquals(formatYaml(YamlValue.String("true")), "\"true\"")
    assertEquals(formatYaml(YamlValue.String("null")), "\"null\"")
    assertEquals(formatYaml(YamlValue.String("key: value")), "\"key: value\"")
  }

  test("format empty sequence") {
    assertEquals(formatYaml(YamlValue.Sequence(List.empty)), "[]")
  }

  test("format sequence") {
    val seq = YamlValue.Sequence(List(YamlValue.Integer(1L), YamlValue.Integer(2L)))
    val result = formatYaml(seq)
    assert(result.contains("- 1"), s"Expected '- 1' in: $result")
    assert(result.contains("- 2"), s"Expected '- 2' in: $result")
  }

  test("format empty mapping") {
    assertEquals(formatYaml(YamlValue.Mapping(Map.empty)), "{}")
  }

  test("format mapping with scalar values") {
    val mapping = YamlValue.Mapping(Map("name" -> YamlValue.String("Alice")))
    val result = formatYaml(mapping)
    assert(result.contains("name: Alice"), s"Expected 'name: Alice' in: $result")
  }

  test("format nested mapping") {
    val inner = YamlValue.Mapping(Map("x" -> YamlValue.Integer(1L)))
    val outer = YamlValue.Mapping(Map("point" -> inner))
    val result = formatYaml(outer)
    assert(result.contains("point:"), s"Expected 'point:' in: $result")
    assert(result.contains("  x: 1"), s"Expected '  x: 1' in: $result")
  }

  // --- audit fixes: key quoting ---

  test("quote and escape mapping keys that break plain YAML") {
    val doc = YamlValue.Mapping(
      Map(
        "a\"b" -> YamlValue.Integer(1),
        "a:b" -> YamlValue.Integer(2),
        "a b" -> YamlValue.Integer(3),
        "a\nb" -> YamlValue.Integer(4)
      )
    )
    val printed = formatYaml(doc)
    assert(printed.contains("\"a\\\"b\": 1"), printed)
    assert(printed.contains("\"a:b\": 2"), printed)
    assert(printed.contains("\"a b\": 3"), printed)
    assert(printed.contains("\"a\\nb\": 4"), printed)

  }
}
