package net.ghoula.sarati.internal

/** ZigZag mapping between signed 64-bit values and their varint-friendly unsigned form: `0`, `-1`,
  * `1`, `-2`, `2`, … map to `0`, `1`, `2`, `3`, `4`, ….
  */
object ZigZag {

  def encode(n: Long): Long = (n << 1) ^ (n >> 63)

  def decode(n: Long): Long = (n >>> 1) ^ -(n & 1)
}
