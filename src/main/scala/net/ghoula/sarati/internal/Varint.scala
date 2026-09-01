package net.ghoula.sarati.internal

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import net.ghoula.sarati.SaratiError

/** Unsigned LEB128 varint encoding: 7-bit groups, least significant first, high bit as the
  * continuation flag. Decoding rejects runs longer than 64 bits (`SaratiError.VarintOverflow`).
  */
object Varint {

  def encodeToArray(value: Long): Array[Byte] = {
    val buffer = ArrayBuffer.empty[Byte]

    @tailrec
    def loop(v: Long): Array[Byte] = {
      val byteVal = (v & 0x7f).toByte
      val nextV = v >>> 7

      nextV match {
        case 0 =>
          buffer += byteVal
          buffer.toArray
        case _ =>
          buffer += (byteVal | 0x80).toByte
          loop(nextV)
      }
    }

    loop(value)
  }

  def encode(value: Long): IArray[Byte] = IArray.unsafeFromArray(encodeToArray(value))

  def decode(bytes: IArray[Byte], offset: Int = 0): Either[SaratiError, (Long, Int)] = {

    @tailrec
    def loop(
      currentIndex: Int,
      currentValue: Long,
      currentShift: Int,
      consumed: Int
    ): Either[SaratiError, (Long, Int)] =
      (currentIndex >= bytes.length, currentShift >= 64) match {
        case (true, _) => Left(SaratiError.Eof(offset + consumed))
        case (_, true) => Left(SaratiError.VarintOverflow)
        case _ =>
          val byte = bytes(currentIndex)
          val nextConsumed = consumed + 1
          val nextValue = currentValue | ((byte & 0x7f).toLong << currentShift)

          (byte & 0x80) match {
            case 0 => Right((nextValue, nextConsumed))
            case _ => loop(currentIndex + 1, nextValue, currentShift + 7, nextConsumed)
          }
      }

    loop(currentIndex = offset, currentValue = 0L, currentShift = 0, consumed = 0)
  }
}
