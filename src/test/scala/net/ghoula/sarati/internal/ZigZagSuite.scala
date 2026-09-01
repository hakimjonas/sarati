package net.ghoula.sarati.internal

class ZigZagSuite extends munit.FunSuite {

  test("encode maps signed to unsigned correctly") {
    assertEquals(ZigZag.encode(0L), 0L)
    assertEquals(ZigZag.encode(-1L), 1L)
    assertEquals(ZigZag.encode(1L), 2L)
    assertEquals(ZigZag.encode(-2L), 3L)
    assertEquals(ZigZag.encode(2L), 4L)
  }

  test("encode boundary values") {
    assertEquals(ZigZag.encode(Long.MinValue), -1L) // 0xFFFFFFFFFFFFFFFF unsigned
    assertEquals(ZigZag.encode(Long.MaxValue), -2L) // 0xFFFFFFFFFFFFFFFE unsigned
  }

  test("decode reverses encode") {
    assertEquals(ZigZag.decode(0L), 0L)
    assertEquals(ZigZag.decode(1L), -1L)
    assertEquals(ZigZag.decode(2L), 1L)
    assertEquals(ZigZag.decode(3L), -2L)
    assertEquals(ZigZag.decode(4L), 2L)
  }

  test("round-trip") {
    val values = Seq(0L, 1L, -1L, 2L, -2L, 127L, -128L, 32767L, -32768L, Long.MinValue, Long.MaxValue)
    values.foreach { v =>
      assertEquals(ZigZag.decode(ZigZag.encode(v)), v)
    }
  }
}
