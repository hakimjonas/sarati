package net.ghoula.sarati.internal

import scala.annotation.tailrec

import net.ghoula.sarati.SaratiError

/** Byte-array assembly and big-endian IEEE 754 conversion. */
object ByteOps {

  def concatArrays(chunks: List[Array[Byte]]): Array[Byte] = {
    val totalLen = chunks.foldLeft(0)(_ + _.length)
    val result = new Array[Byte](totalLen)

    @tailrec
    def copyChunks(remaining: List[Array[Byte]], pos: Int): Unit = remaining match {
      case Nil => ()
      case chunk :: rest =>
        System.arraycopy(chunk, 0, result, pos, chunk.length)
        copyChunks(rest, pos + chunk.length)
    }

    copyChunks(chunks, 0)
    result
  }

  def concatBytes(chunks: List[IArray[Byte]]): IArray[Byte] = {
    val totalLen = chunks.foldLeft(0)(_ + _.length)
    val result = new Array[Byte](totalLen)

    @tailrec
    def copyChunks(remaining: List[IArray[Byte]], pos: Int): Unit = remaining match {
      case Nil => ()
      case chunk :: rest =>
        chunk.copyToArray(result, pos, chunk.length)
        copyChunks(rest, pos + chunk.length)
    }

    copyChunks(chunks, 0)
    IArray.unsafeFromArray(result)
  }

  def encodeDouble(d: Double): IArray[Byte] = {
    val bits = java.lang.Double.doubleToLongBits(d)
    IArray(
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

  def decodeDouble(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Double, Int)] =
    (offset + 8 <= bytes.length) match {
      case false => Left(SaratiError.Eof(offset))
      case true =>
        val bits: Long =
          ((bytes(offset).toLong & 0xff) << 56) |
            ((bytes(offset + 1).toLong & 0xff) << 48) |
            ((bytes(offset + 2).toLong & 0xff) << 40) |
            ((bytes(offset + 3).toLong & 0xff) << 32) |
            ((bytes(offset + 4).toLong & 0xff) << 24) |
            ((bytes(offset + 5).toLong & 0xff) << 16) |
            ((bytes(offset + 6).toLong & 0xff) << 8) |
            (bytes(offset + 7).toLong & 0xff)
        Right((java.lang.Double.longBitsToDouble(bits), 8))
    }

  def encodeFloat(f: Float): IArray[Byte] = {
    val bits = java.lang.Float.floatToIntBits(f)
    IArray(
      (bits >>> 24).toByte,
      (bits >>> 16).toByte,
      (bits >>> 8).toByte,
      bits.toByte
    )
  }

  def decodeFloat(bytes: IArray[Byte], offset: Int): Either[SaratiError, (Float, Int)] =
    (offset + 4 <= bytes.length) match {
      case false => Left(SaratiError.Eof(offset))
      case true =>
        val bits: Int =
          ((bytes(offset).toInt & 0xff) << 24) |
            ((bytes(offset + 1).toInt & 0xff) << 16) |
            ((bytes(offset + 2).toInt & 0xff) << 8) |
            (bytes(offset + 3).toInt & 0xff)
        Right((java.lang.Float.intBitsToFloat(bits), 4))
    }
}
