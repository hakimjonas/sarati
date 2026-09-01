package net.ghoula.sarati

import net.ghoula.sarati.internal.ByteOps

class SaratiCodecSuite extends munit.FunSuite {

  private def roundTrip[A](value: A)(using codec: SaratiCodec[A]): Unit = {
    val encoded = codec.encode(value)
    val Right((decoded, consumed)) = codec.decode(encoded): @unchecked
    assertEquals(decoded, value)
    assertEquals(consumed, encoded.length)
  }

  // --- Primitive round-trips ---

  test("Long round-trip") {
    Seq(0L, 1L, -1L, 127L, -128L, 32767L, -32768L, Long.MinValue, Long.MaxValue).foreach(roundTrip(_))
  }

  test("Int round-trip") {
    Seq(0, 1, -1, 127, -128, 32767, -32768, Int.MinValue, Int.MaxValue).foreach(roundTrip(_))
  }

  test("Short round-trip") {
    Seq(0.toShort, 1.toShort, (-1).toShort, Short.MinValue, Short.MaxValue).foreach(roundTrip(_))
  }

  test("Byte round-trip") {
    Seq(0.toByte, 1.toByte, (-1).toByte, Byte.MinValue, Byte.MaxValue).foreach(roundTrip(_))
  }

  test("Boolean round-trip") {
    roundTrip(true)
    roundTrip(false)
  }

  test("Double round-trip") {
    Seq(0.0, 1.0, -1.0, 3.14159, Double.MinValue, Double.MaxValue).foreach(roundTrip(_))
  }

  test("Float round-trip") {
    Seq(0.0f, 1.0f, -1.0f, 3.14f, Float.MinValue, Float.MaxValue).foreach(roundTrip(_))
  }

  test("String round-trip") {
    Seq("", "hello", "Hello, World!", "\u00e9\u00e8\u00ea", "a" * 1000).foreach(roundTrip(_))
  }

  test("Option round-trip") {
    roundTrip(Option.empty[Int])
    roundTrip(Option(42))
    roundTrip(Option("hello"))
  }

  test("List round-trip") {
    roundTrip(List.empty[Int])
    roundTrip(List(1, 2, 3))
    roundTrip(List("a", "bb", "ccc"))
  }

  test("IArray[Byte] round-trip") {
    val codec = summon[SaratiCodec[IArray[Byte]]]

    val empty = IArray.empty[Byte]
    val Right((d1, c1)) = codec.decode(codec.encode(empty)): @unchecked
    assertEquals(d1.toSeq, empty.toSeq)
    assert(c1 > 0)

    val data = IArray[Byte](1, 2, 3, 4, 5)
    val Right((d2, c2)) = codec.decode(codec.encode(data)): @unchecked
    assertEquals(d2.toSeq, data.toSeq)
    assert(c2 > 0)
  }

  // --- Derived product codecs ---

  test("derived empty case class") {
    case class Empty() derives SaratiCodec
    val codec = summon[SaratiCodec[Empty]]
    val encoded = codec.encode(Empty())
    assertEquals(encoded.length, 0)
    val Right((decoded, consumed)) = codec.decode(encoded): @unchecked
    assertEquals(decoded, Empty())
    assertEquals(consumed, 0)
  }

  test("derived single-field case class") {
    case class Wrapper(value: Int) derives SaratiCodec
    roundTrip(Wrapper(42))(using summon[SaratiCodec[Wrapper]])
    roundTrip(Wrapper(0))(using summon[SaratiCodec[Wrapper]])
    roundTrip(Wrapper(-1))(using summon[SaratiCodec[Wrapper]])
  }

  test("derived multi-field case class") {
    case class Point(x: Int, y: Int) derives SaratiCodec
    roundTrip(Point(1, 2))(using summon[SaratiCodec[Point]])
    roundTrip(Point(0, 0))(using summon[SaratiCodec[Point]])
    roundTrip(Point(-100, 200))(using summon[SaratiCodec[Point]])
  }

  test("derived nested case class") {
    case class Inner(n: Int) derives SaratiCodec
    case class Outer(label: String, inner: Inner) derives SaratiCodec
    roundTrip(Outer("test", Inner(42)))(using summon[SaratiCodec[Outer]])
  }

  test("derived case class with Option fields") {
    case class OptFields(name: String, age: Option[Int]) derives SaratiCodec
    roundTrip(OptFields("Alice", Some(30)))(using summon[SaratiCodec[OptFields]])
    roundTrip(OptFields("Bob", None))(using summon[SaratiCodec[OptFields]])
  }

  test("derived case class with List fields") {
    case class WithList(tags: List[String], counts: List[Int]) derives SaratiCodec
    roundTrip(WithList(List("a", "b"), List(1, 2, 3)))(using summon[SaratiCodec[WithList]])
    roundTrip(WithList(Nil, Nil))(using summon[SaratiCodec[WithList]])
  }

  test("DomainState smoke test") {
    case class DomainState(etag: String, nextFetch: Long) derives SaratiCodec
    val state = DomainState("W/\"abc123\"", 1709337600000L)
    roundTrip(state)(using summon[SaratiCodec[DomainState]])
  }

  // --- Derived sum (enum) codecs ---

  test("derived enum with parameterized and parameterless cases") {
    enum Shape {
      case Circle(radius: Double)
      case Rectangle(width: Double, height: Double)
      case Point
    }
    object Shape {
      given SaratiCodec[Shape.Circle] = SaratiCodec.derived
      given SaratiCodec[Shape.Rectangle] = SaratiCodec.derived
      given SaratiCodec[Shape.Point.type] = SaratiCodec.derived
      given SaratiCodec[Shape] = SaratiCodec.derived
    }

    val codec = summon[SaratiCodec[Shape]]
    val Right((c, cc)) = codec.decode(codec.encode(Shape.Circle(3.14))): @unchecked
    assertEquals(c, Shape.Circle(3.14))
    assert(cc > 0)

    val Right((r, rc)) = codec.decode(codec.encode(Shape.Rectangle(2.0, 4.0))): @unchecked
    assertEquals(r, Shape.Rectangle(2.0, 4.0))
    assert(rc > 0)

    val Right((p, pc)) = codec.decode(codec.encode(Shape.Point)): @unchecked
    assertEquals(p, Shape.Point)
    assert(pc > 0)
  }

  test("enum ordinal round-trip preserves variant identity") {
    enum Color {
      case Red, Green, Blue
    }
    object Color {
      given SaratiCodec[Color.Red.type] = SaratiCodec.derived
      given SaratiCodec[Color.Green.type] = SaratiCodec.derived
      given SaratiCodec[Color.Blue.type] = SaratiCodec.derived
      given SaratiCodec[Color] = SaratiCodec.derived
    }

    val codec = summon[SaratiCodec[Color]]
    Seq(Color.Red, Color.Green, Color.Blue).foreach { color =>
      val encoded = codec.encode(color)
      val Right((decoded, consumed)) = codec.decode(encoded): @unchecked
      assertEquals(decoded, color)
      assertEquals(consumed, encoded.length)
    }
  }

  // --- Offset tracking ---

  test("decode from middle of byte array") {
    val codec = summon[SaratiCodec[Int]]
    val prefix = IArray[Byte](0xff.toByte, 0xff.toByte)
    val encoded = codec.encode(42)
    val combined = IArray.unsafeFromArray(prefix.toSeq.toArray ++ encoded.toSeq.toArray)
    val Right((value, consumed)) = codec.decode(combined, offset = 2): @unchecked
    assertEquals(value, 42)
    assertEquals(consumed, encoded.length)
  }

  test("decode preserves trailing bytes") {
    val codec = summon[SaratiCodec[Int]]
    val encoded = codec.encode(42)
    val trailing = IArray[Byte](0xaa.toByte, 0xbb.toByte)
    val combined = IArray.unsafeFromArray(encoded.toSeq.toArray ++ trailing.toSeq.toArray)
    val Right((value, consumed)) = codec.decode(combined): @unchecked
    assertEquals(value, 42)
    assertEquals(consumed, encoded.length)
    assert(consumed < combined.length)
  }

  // --- Error cases ---

  test("truncated bytes produce Eof") {
    val codec = summon[SaratiCodec[Int]]
    assertEquals(codec.decode(IArray.empty[Byte]), Left(SaratiError.Eof(0)))
  }

  test("invalid boolean tag produces ParseError") {
    val codec = summon[SaratiCodec[Boolean]]
    val result = codec.decode(IArray(0x02.toByte))
    result match {
      case Left(SaratiError.ParseError(_)) => () // expected
      case other => fail(s"Expected ParseError, got $other")
    }
  }

  test("invalid option tag produces ParseError") {
    val codec = summon[SaratiCodec[Option[Int]]]
    val result = codec.decode(IArray(0x02.toByte))
    result match {
      case Left(SaratiError.ParseError(_)) => () // expected
      case other => fail(s"Expected ParseError, got $other")
    }
  }

  test("invalid enum ordinal produces ParseError") {
    enum Tiny {
      case A
    }
    object Tiny {
      given SaratiCodec[Tiny.A.type] = SaratiCodec.derived
      given SaratiCodec[Tiny] = SaratiCodec.derived
    }

    val codec = summon[SaratiCodec[Tiny]]
    // Encode ordinal 99 (invalid) followed by empty payload
    val badOrdinal = summon[SaratiCodec[Int]].encode(99)
    val result = codec.decode(badOrdinal)
    result match {
      case Left(SaratiError.ParseError(_)) => () // expected
      case other => fail(s"Expected ParseError, got $other")
    }
  }

  // --- envelope ---

  test("envelope round-trip") {
    val framed = SaratiCodec.encodeEnvelope[Int](42)
    (0 until 4).foreach(i => assertEquals(framed(i), Sarati.Magic(i)))
    SaratiCodec.decodeEnvelope[Int](framed) match {
      case Right(42) => ()
      case other => fail(s"expected Right(42), got $other")
    }
  }

  test("envelope rejects altered magic") {
    val framed = SaratiCodec.encodeEnvelope[Int](42)
    val bad = framed.updated(1, 0x00.toByte)
    SaratiCodec.decodeEnvelope[Int](bad) match {
      case Left(SaratiError.InvalidMagic(found)) => assertEquals(found.length, 4)
      case other => fail(s"expected InvalidMagic, got $other")
    }
  }

  test("envelope rejects truncated frames whose prefix matches") {
    val short = Sarati.Magic.slice(0, 2)
    SaratiCodec.decodeEnvelope[Int](short) match {
      case Left(SaratiError.Eof(_)) => ()
      case other => fail(s"expected Eof, got $other")
    }
  }

  test("envelope rejects truncated frames whose prefix differs") {
    SaratiCodec.decodeEnvelope[Int](IArray[Byte](0x53, 0x00)) match {
      case Left(SaratiError.InvalidMagic(_)) => ()
      case other => fail(s"expected InvalidMagic, got $other")
    }
  }

  test("envelope rejects a foreign version") {
    val foreignVersion = ByteOps.concatBytes(
      List(Sarati.Magic, IArray.unsafeFromArray(summon[SaratiCodec[Int]].encodeToArray(99)))
    )
    SaratiCodec.decodeEnvelope[Int](foreignVersion) match {
      case Left(SaratiError.VersionMismatch(expected, found)) =>
        assertEquals(expected, Sarati.Version)
        assertEquals(found, 99)
      case other => fail(s"expected VersionMismatch, got $other")
    }
  }

  test("envelope ignores trailing bytes after the payload") {
    val framed = ByteOps.concatBytes(List(SaratiCodec.encodeEnvelope[Int](42), IArray[Byte](0x00, 0x01)))
    SaratiCodec.decodeEnvelope[Int](framed) match {
      case Right(42) => ()
      case other => fail(s"expected Right(42), got $other")
    }
  }

  test("envelope round-trips a derived case class") {
    case class Framed(id: Long, tag: String) derives SaratiCodec
    val framed = SaratiCodec.encodeEnvelope[Framed](Framed(7, "x"))
    SaratiCodec.decodeEnvelope[Framed](framed) match {
      case Right(Framed(7, "x")) => ()
      case other => fail(s"expected Right(Framed(7, x)), got $other")
    }

  }
}
