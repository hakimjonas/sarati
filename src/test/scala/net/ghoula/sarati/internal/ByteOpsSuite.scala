package net.ghoula.sarati.internal

import net.ghoula.sarati.SaratiError

class ByteOpsSuite extends munit.FunSuite {

  // --- concatBytes ---

  test("concatBytes empty list") {
    assertEquals(ByteOps.concatBytes(Nil).toSeq, Seq.empty[Byte])
  }

  test("concatBytes single chunk") {
    val chunk = IArray[Byte](1, 2, 3)
    assertEquals(ByteOps.concatBytes(List(chunk)).toSeq, Seq[Byte](1, 2, 3))
  }

  test("concatBytes multiple chunks") {
    val a = IArray[Byte](1, 2)
    val b = IArray[Byte](3, 4, 5)
    val c = IArray(6.toByte)
    assertEquals(ByteOps.concatBytes(List(a, b, c)).toSeq, Seq[Byte](1, 2, 3, 4, 5, 6))
  }

  test("concatBytes with empty chunks") {
    val a = IArray.empty[Byte]
    val b = IArray[Byte](1, 2)
    val c = IArray.empty[Byte]
    assertEquals(ByteOps.concatBytes(List(a, b, c)).toSeq, Seq[Byte](1, 2))
  }

  // --- Double ---

  test("double round-trip normal values") {
    val values = Seq(0.0, 1.0, -1.0, 3.14159, Double.MinValue, Double.MaxValue)
    values.foreach { d =>
      val encoded = ByteOps.encodeDouble(d)
      assertEquals(encoded.length, 8)
      val Right((decoded, consumed)) = ByteOps.decodeDouble(encoded, 0): @unchecked
      assertEquals(decoded, d)
      assertEquals(consumed, 8)
    }
  }

  test("double special values") {
    val encoded = ByteOps.encodeDouble(Double.NaN)
    val Right((decoded, _)) = ByteOps.decodeDouble(encoded, 0): @unchecked
    assert(decoded.isNaN)

    Seq(Double.PositiveInfinity, Double.NegativeInfinity).foreach { d =>
      val enc = ByteOps.encodeDouble(d)
      val Right((dec, _)) = ByteOps.decodeDouble(enc, 0): @unchecked
      assertEquals(dec, d)
    }
  }

  test("double negative zero") {
    val encoded = ByteOps.encodeDouble(-0.0)
    val Right((decoded, _)) = ByteOps.decodeDouble(encoded, 0): @unchecked
    assertEquals(java.lang.Double.doubleToLongBits(decoded), java.lang.Double.doubleToLongBits(-0.0))
  }

  test("decodeDouble eof") {
    assertEquals(ByteOps.decodeDouble(IArray.empty[Byte], 0), Left(SaratiError.Eof(0)))
    assertEquals(ByteOps.decodeDouble(IArray[Byte](1, 2, 3), 0), Left(SaratiError.Eof(0)))
  }

  // --- Float ---

  test("float round-trip normal values") {
    val values = Seq(0.0f, 1.0f, -1.0f, 3.14f, Float.MinValue, Float.MaxValue)
    values.foreach { f =>
      val encoded = ByteOps.encodeFloat(f)
      assertEquals(encoded.length, 4)
      val Right((decoded, consumed)) = ByteOps.decodeFloat(encoded, 0): @unchecked
      assertEquals(decoded, f)
      assertEquals(consumed, 4)
    }
  }

  test("float special values") {
    val encoded = ByteOps.encodeFloat(Float.NaN)
    val Right((decoded, _)) = ByteOps.decodeFloat(encoded, 0): @unchecked
    assert(decoded.isNaN)

    Seq(Float.PositiveInfinity, Float.NegativeInfinity).foreach { f =>
      val enc = ByteOps.encodeFloat(f)
      val Right((dec, _)) = ByteOps.decodeFloat(enc, 0): @unchecked
      assertEquals(dec, f)
    }
  }

  test("decodeFloat eof") {
    assertEquals(ByteOps.decodeFloat(IArray.empty[Byte], 0), Left(SaratiError.Eof(0)))
    assertEquals(ByteOps.decodeFloat(IArray[Byte](1, 2, 3), 0), Left(SaratiError.Eof(0)))
  }
}
