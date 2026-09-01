package net.ghoula.sarati

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.deriving.Mirror

import net.ghoula.sarati.internal.{ByteOps, Varint, ZigZag}

object Sarati {
  val Magic: IArray[Byte] = IArray(0x53, 0x52, 0x54, 0x49)
  val Version: Int = 1
}

/** Wire-format errors. `Eof` reports the position the decode reached; `VarintOverflow` rejects
  * varints longer than 64 bits; `ParseError` covers invalid structure such as unknown option,
  * boolean, or enum-ordinal tags.
  */
enum SaratiError(val message: String) extends Exception(message) {
  case InvalidMagic(found: IArray[Byte]) extends SaratiError("Invalid magic bytes")
  case VersionMismatch(expected: Int, found: Int)
      extends SaratiError(s"Version mismatch. Expected $expected, got $found")
  case Eof(position: Int) extends SaratiError(s"Unexpected end of input at position $position")
  case VarintOverflow extends SaratiError("Varint exceeded 64 bits")
  case ParseError(msg: String) extends SaratiError(msg)
}

/** Binary codec for `A`: encodes to bytes and decodes at an offset.
  *
  * `encode` produces the value's complete byte representation; `decode(bytes, offset)` reads one
  * value starting at `offset` and returns it with the number of bytes consumed, so a codec can read
  * a field out of a larger buffer and leave the caller positioned at the next field. The wire
  * format is positional: products are their fields concatenated in declaration order, sums are
  * ordinal plus payload, and no field or case names are encoded — renaming or reordering fields or
  * cases changes the format, while appending enum cases at the end preserves it.
  *
  * Instances for primitives, `Option`, `List`, and `IArray[Byte]` are in [[SaratiCodec]]; case
  * class and enum instances derive at compile time via `SaratiCodec.derived` (no runtime
  * reflection).
  */
trait SaratiCodec[A] {
  def encodeToArray(a: A): Array[Byte]
  final def encode(a: A): IArray[Byte] = IArray.unsafeFromArray(encodeToArray(a))
  def decode(bytes: IArray[Byte], offset: Int = 0): Either[SaratiError, (A, Int)]
}

/** Primitive and collection codecs, plus compile-time derivation.
  *
  * Encodings: `Long`/`Int`/`Short` are ZigZag-mapped and then LEB128 varint; `Byte` is one raw
  * byte; `Boolean` is `0x00`/`0x01`; `Double`/`Float` are IEEE 754 bits, big-endian, 8 and 4 bytes;
  * `String` is a varint byte length followed by UTF-8 bytes; `Option` is a `0x00`/`0x01` tag plus
  * the value when present; `List` is a varint element count followed by the elements;
  * `IArray[Byte]` is a varint length followed by the bytes.
  */
object SaratiCodec {

  inline def derived[T](using m: Mirror.Of[T]): SaratiCodec[T] =
    inline m match {
      case s: Mirror.SumOf[T] => derivedSum[T](using s)
      case p: Mirror.ProductOf[T] => derivedProduct[T](using p)
    }

  private inline def derivedProduct[T](using m: Mirror.ProductOf[T]): SaratiCodec[T] =
    ${ internal.SaratiDerivation.deriveProductCodecImpl[T, m.MirroredElemTypes, m.MirroredElemLabels]('m) }

  private inline def derivedSum[T](using m: Mirror.SumOf[T]): SaratiCodec[T] =
    ${ internal.SaratiDerivation.deriveSumCodecImpl[T, m.MirroredElemTypes, m.MirroredElemLabels]('m) }

  // --- Primitive codecs ---

  given SaratiCodec[Long] = new SaratiCodec[Long] {
    def encodeToArray(a: Long): Array[Byte] = Varint.encodeToArray(ZigZag.encode(a))
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Long, Int)] =
      Varint.decode(bytes, offset) match {
        case Left(err) => Left(err)
        case Right((raw, consumed)) => Right((ZigZag.decode(raw), consumed))
      }
  }

  given SaratiCodec[Int] = new SaratiCodec[Int] {
    def encodeToArray(a: Int): Array[Byte] = Varint.encodeToArray(ZigZag.encode(a.toLong))
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Int, Int)] =
      Varint.decode(bytes, offset) match {
        case Left(err) => Left(err)
        case Right((raw, consumed)) => Right((ZigZag.decode(raw).toInt, consumed))
      }
  }

  given SaratiCodec[Short] = new SaratiCodec[Short] {
    def encodeToArray(a: Short): Array[Byte] = Varint.encodeToArray(ZigZag.encode(a.toLong))
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Short, Int)] =
      Varint.decode(bytes, offset) match {
        case Left(err) => Left(err)
        case Right((raw, consumed)) => Right((ZigZag.decode(raw).toShort, consumed))
      }
  }

  given SaratiCodec[Byte] = new SaratiCodec[Byte] {
    def encodeToArray(a: Byte): Array[Byte] = Array(a)
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Byte, Int)] =
      (offset < bytes.length) match {
        case true => Right((bytes(offset), 1))
        case false => Left(SaratiError.Eof(offset))
      }
  }

  given SaratiCodec[Boolean] = new SaratiCodec[Boolean] {
    def encodeToArray(a: Boolean): Array[Byte] = a match {
      case true => Array(0x01.toByte)
      case false => Array(0x00.toByte)
    }
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Boolean, Int)] =
      (offset < bytes.length) match {
        case false => Left(SaratiError.Eof(offset))
        case true =>
          bytes(offset) match {
            case 0x00 => Right((false, 1))
            case 0x01 => Right((true, 1))
            case tag => Left(SaratiError.ParseError(s"Invalid boolean tag: $tag"))
          }
      }
  }

  given SaratiCodec[Double] = new SaratiCodec[Double] {
    def encodeToArray(a: Double): Array[Byte] = {
      val bits = java.lang.Double.doubleToLongBits(a)
      Array(
        (bits >>> 56).toByte,
        (bits >>> 48).toByte,
        (bits >>> 40).toByte,
        (bits >>> 32).toByte,
        (bits >>> 24).toByte,
        (bits >>> 16).toByte,
        (bits >>> 8).toByte,
        bits.toByte
      )
    }
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Double, Int)] =
      ByteOps.decodeDouble(bytes, offset)
  }

  given SaratiCodec[Float] = new SaratiCodec[Float] {
    def encodeToArray(a: Float): Array[Byte] = {
      val bits = java.lang.Float.floatToIntBits(a)
      Array(
        (bits >>> 24).toByte,
        (bits >>> 16).toByte,
        (bits >>> 8).toByte,
        bits.toByte
      )
    }
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Float, Int)] =
      ByteOps.decodeFloat(bytes, offset)
  }

  given SaratiCodec[String] = new SaratiCodec[String] {
    def encodeToArray(a: String): Array[Byte] = {
      val utf8Bytes: Array[Byte] = a.getBytes(StandardCharsets.UTF_8).nn
      val lenBytes: Array[Byte] = Varint.encodeToArray(utf8Bytes.length.toLong)
      ByteOps.concatArrays(List(lenBytes, utf8Bytes))
    }
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (String, Int)] =
      Varint.decode(bytes, offset) match {
        case Left(err) => Left(err)
        case Right((len, lenConsumed)) =>
          val strLen: Int = len.toInt
          val strStart: Int = offset + lenConsumed
          (strStart + strLen <= bytes.length) match {
            case false => Left(SaratiError.Eof(strStart))
            case true =>
              val rawArray = bytes.asInstanceOf[Array[Byte]] // scalafix:ok DisableSyntax.asInstanceOf
              Right((new String(rawArray, strStart, strLen, StandardCharsets.UTF_8), lenConsumed + strLen))
          }
      }
  }

  given optionCodec[A](using inner: SaratiCodec[A]): SaratiCodec[Option[A]] =
    new SaratiCodec[Option[A]] {
      def encodeToArray(a: Option[A]): Array[Byte] = a match {
        case None => Array(0x00.toByte)
        case Some(v) => ByteOps.concatArrays(List(Array(0x01.toByte), inner.encodeToArray(v)))
      }
      def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Option[A], Int)] =
        (offset < bytes.length) match {
          case false => Left(SaratiError.Eof(offset))
          case true =>
            bytes(offset) match {
              case 0x00 => Right((None, 1))
              case 0x01 =>
                inner.decode(bytes, offset + 1) match {
                  case Left(err) => Left(err)
                  case Right((v, consumed)) => Right((Some(v), 1 + consumed))
                }
              case tag => Left(SaratiError.ParseError(s"Invalid option tag: $tag"))
            }
        }
    }

  given listCodec[A](using inner: SaratiCodec[A]): SaratiCodec[List[A]] =
    new SaratiCodec[List[A]] {
      def encodeToArray(a: List[A]): Array[Byte] = {
        val countBytes: Array[Byte] = Varint.encodeToArray(a.length.toLong)
        val elemChunks: List[Array[Byte]] = a.map(inner.encodeToArray)
        ByteOps.concatArrays(countBytes :: elemChunks)
      }
      def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (List[A], Int)] =
        Varint.decode(bytes, offset) match {
          case Left(err) => Left(err)
          case Right((count, countConsumed)) =>
            @tailrec
            def loop(
              remaining: Int,
              currentOffset: Int,
              totalConsumed: Int,
              acc: List[A]
            ): Either[SaratiError, (List[A], Int)] =
              (remaining <= 0) match {
                case true => Right((acc.reverse, totalConsumed))
                case false =>
                  inner.decode(bytes, currentOffset) match {
                    case Left(err) => Left(err)
                    case Right((v, consumed)) =>
                      loop(remaining - 1, currentOffset + consumed, totalConsumed + consumed, v :: acc)
                  }
              }
            loop(count.toInt, offset + countConsumed, countConsumed, Nil)
        }
    }

  given SaratiCodec[IArray[Byte]] = new SaratiCodec[IArray[Byte]] {
    def encodeToArray(a: IArray[Byte]): Array[Byte] = {
      val lenBytes: Array[Byte] = Varint.encodeToArray(a.length.toLong)
      val result = new Array[Byte](lenBytes.length + a.length)
      System.arraycopy(lenBytes, 0, result, 0, lenBytes.length)
      a.copyToArray(result, lenBytes.length, a.length)
      result
    }
    def decode(bytes: IArray[Byte], offset: Int): Either[SaratiError, (IArray[Byte], Int)] =
      Varint.decode(bytes, offset) match {
        case Left(err) => Left(err)
        case Right((len, lenConsumed)) =>
          val dataLen: Int = len.toInt
          val dataStart: Int = offset + lenConsumed
          (dataStart + dataLen <= bytes.length) match {
            case false => Left(SaratiError.Eof(dataStart))
            case true =>
              Right((bytes.slice(dataStart, dataStart + dataLen), lenConsumed + dataLen))
          }
      }
  }

  /** Frames `a` with the sarati envelope: the 4-byte magic (`SRTI`), the current format version
    * ([[Sarati.Version]], encoded with the `Int` codec), then the encoded value. Enveloped and raw
    * encodings are distinct — [[decode]] reads raw payloads, [[decodeEnvelope]] reads frames.
    */
  def encodeEnvelope[A](a: A)(using codec: SaratiCodec[A]): IArray[Byte] =
    ByteOps.concatBytes(
      List(
        Sarati.Magic,
        IArray.unsafeFromArray(summon[SaratiCodec[Int]].encodeToArray(Sarati.Version)),
        IArray.unsafeFromArray(codec.encodeToArray(a))
      )
    )

  /** Reads an [[encodeEnvelope]] frame: verifies the magic bytes (`InvalidMagic` on mismatch, even
    * for a truncated frame whose available prefix differs), checks the format version
    * (`VersionMismatch`), and decodes the payload. Returns the value; trailing bytes after the
    * payload are ignored, matching [[SaratiCodec.decode]].
    */
  def decodeEnvelope[A](bytes: IArray[Byte])(using codec: SaratiCodec[A]): Either[SaratiError, A] =
    magicOffset(bytes) match {
      case Left(err) => Left(err)
      case Right(offset) =>
        summon[SaratiCodec[Int]].decode(bytes, offset) match {
          case Left(err) => Left(err)
          case Right((version, consumed)) =>
            (version == Sarati.Version) match {
              case true => codec.decode(bytes, offset + consumed).map(_._1)
              case false => Left(SaratiError.VersionMismatch(Sarati.Version, version))
            }
        }
    }

  /** Verifies the frame's magic prefix; `Right` is the offset just past the magic. */
  private def magicOffset(bytes: IArray[Byte]): Either[SaratiError, Int] = {
    val magicLen = Sarati.Magic.length
    val available = math.min(magicLen, bytes.length)
    val firstMismatch = (0 until available).find(i => bytes(i) != Sarati.Magic(i))
    (firstMismatch, available == magicLen) match {
      case (Some(_), _) => Left(SaratiError.InvalidMagic(bytes.slice(0, available)))
      case (None, true) => Right(magicLen)
      case (None, false) => Left(SaratiError.Eof(bytes.length))
    }
  }
}
