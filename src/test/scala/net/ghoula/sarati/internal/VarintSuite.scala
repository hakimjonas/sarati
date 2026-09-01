package net.ghoula.sarati.internal

import net.ghoula.sarati.SaratiError

class VarintSuite extends munit.FunSuite {

  test("encode single-byte values") {
    assertEquals(Varint.encode(0L).toSeq, Seq[Byte](0x00))
    assertEquals(Varint.encode(1L).toSeq, Seq[Byte](0x01))
    assertEquals(Varint.encode(127L).toSeq, Seq[Byte](0x7f))
  }

  test("encode multi-byte values") {
    assertEquals(Varint.encode(128L).toSeq, Seq[Byte](0x80.toByte, 0x01))
    assertEquals(Varint.encode(255L).toSeq, Seq[Byte](0xff.toByte, 0x01))
    assertEquals(Varint.encode(256L).toSeq, Seq[Byte](0x80.toByte, 0x02))
    assertEquals(Varint.encode(300L).toSeq, Seq[Byte](0xac.toByte, 0x02))
    assertEquals(Varint.encode(16384L).toSeq, Seq[Byte](0x80.toByte, 0x80.toByte, 0x01))
  }

  test("decode single-byte values") {
    assertEquals(Varint.decode(IArray(0x00)), Right((0L, 1)))
    assertEquals(Varint.decode(IArray(0x01)), Right((1L, 1)))
    assertEquals(Varint.decode(IArray(0x7f)), Right((127L, 1)))
  }

  test("decode multi-byte values") {
    assertEquals(Varint.decode(IArray(0x80.toByte, 0x01.toByte)), Right((128L, 2)))
    assertEquals(Varint.decode(IArray(0xff.toByte, 0x01.toByte)), Right((255L, 2)))
    assertEquals(Varint.decode(IArray(0x80.toByte, 0x02.toByte)), Right((256L, 2)))
    assertEquals(Varint.decode(IArray(0xac.toByte, 0x02.toByte)), Right((300L, 2)))
    assertEquals(Varint.decode(IArray(0x80.toByte, 0x80.toByte, 0x01.toByte)), Right((16384L, 3)))
  }

  test("round-trip") {
    val values = Seq(0L, 1L, 127L, 128L, 255L, 256L, 300L, 1000L, 10000L, 100000L, 1000000L, Long.MaxValue)
    values.foreach { v =>
      val encoded = Varint.encode(v)
      val Right((decoded, consumed)) = Varint.decode(encoded): @unchecked
      assertEquals(decoded, v)
      assertEquals(consumed, encoded.length)
    }
  }

  test("decode with trailing bytes consumes only varint") {
    val bytes = IArray[Byte](0x7f, 0xff.toByte, 0xff.toByte)
    val Right((value, consumed)) = Varint.decode(bytes): @unchecked
    assertEquals(value, 127L)
    assertEquals(consumed, 1)
  }

  test("decode with offset") {
    val bytes = IArray[Byte](0xff.toByte, 0xac.toByte, 0x02)
    val Right((value, consumed)) = Varint.decode(bytes, offset = 1): @unchecked
    assertEquals(value, 300L)
    assertEquals(consumed, 2)
  }

  test("decode unexpected eof") {
    assertEquals(Varint.decode(IArray(0x80.toByte)), Left(SaratiError.Eof(1)))
    assertEquals(Varint.decode(IArray(0x80.toByte, 0x80.toByte)), Left(SaratiError.Eof(2)))
  }

  test("decode empty input") {
    assertEquals(Varint.decode(IArray.empty[Byte]), Left(SaratiError.Eof(0)))
  }
}
