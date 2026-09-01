package net.ghoula.sarati.ast.toml

import munit.FunSuite

class FormatTomlTests extends FunSuite {

  test("format top-level pairs") {
    val doc: TomlDocument = (
      isArrayTable = false,
      pairs = Map("a" -> TomlValue.Integer(1), "b" -> TomlValue.String("x")),
      subtables = Map.empty
    )
    assertEquals(formatToml(doc), "a = 1\nb = \"x\"")
  }

  test("render subtables under headers, array tables as doubled headers") {
    val doc: TomlDocument = (
      isArrayTable = false,
      pairs = Map("top" -> TomlValue.Integer(1)),
      subtables = Map(
        "regular" -> List(
          (isArrayTable = false, pairs = Map("x" -> TomlValue.Integer(2)), subtables = Map.empty)
        ),
        "repeated" -> List(
          arrayTable(pairs = Map("n" -> TomlValue.Integer(1))),
          arrayTable(pairs = Map("n" -> TomlValue.Integer(2)))
        )
      )
    )
    val printed = formatToml(doc)
    val expected =
      """top = 1
        |
        |[regular]
        |x = 2
        |
        |[[repeated]]
        |n = 1
        |
        |[[repeated]]
        |n = 2""".stripMargin
    assertEquals(printed, expected)
  }

  test("render nested subtables with dotted headers") {
    val doc: TomlDocument = (
      isArrayTable = false,
      pairs = Map.empty,
      subtables = Map(
        "a" -> List(
          (
            isArrayTable = false,
            pairs = Map.empty,
            subtables =
              Map("b" -> List((isArrayTable = false, pairs = Map("k" -> TomlValue.Integer(1)), subtables = Map.empty)))
          )
        )
      )
    )
    val printed = formatToml(doc)
    assert(printed.contains("[a.b]"), printed)
    assert(printed.contains("k = 1"), printed)
  }

  test("quote keys that are not bare keys") {
    val doc: TomlDocument = (
      isArrayTable = false,
      pairs = Map("has space" -> TomlValue.Integer(1), "has.dot" -> TomlValue.Integer(2)),
      subtables =
        Map("tbl name" -> List((isArrayTable = false, pairs = Map("k" -> TomlValue.Integer(3)), subtables = Map.empty)))
    )
    val printed = formatToml(doc)
    assert(printed.contains("\"has space\" = 1"), printed)
    assert(printed.contains("\"has.dot\" = 2"), printed)
    assert(printed.contains("[\"tbl name\"]"), printed)
  }

  test("quote non-bare keys inside inline tables") {
    val table = TomlValue.InlineTable(Map("has space" -> TomlValue.Integer(1), "plain" -> TomlValue.Integer(2)))
    assertEquals(formatTomlValue(table), "{ \"has space\" = 1, plain = 2 }")
  }

  test("format floats with TOML nan and infinity spellings") {
    assertEquals(formatTomlValue(TomlValue.Float(Double.NaN)), "nan")
    assertEquals(formatTomlValue(TomlValue.Float(Double.PositiveInfinity)), "inf")
    assertEquals(formatTomlValue(TomlValue.Float(Double.NegativeInfinity)), "-inf")
  }
}
