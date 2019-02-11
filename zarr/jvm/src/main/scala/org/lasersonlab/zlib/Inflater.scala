package org.lasersonlab.zlib

import scala.Array.fill

case object Inflater {
  // Exponentially increase attempted output buffer size by this factor every attempt
  val SCALING_POWER = 8

  def scale(previous: Int): Int =
    if (previous > Int.MaxValue / SCALING_POWER)
      Int.MaxValue
    else
      previous * SCALING_POWER

  def scaleBuffer(previous: Array[Byte]): Array[Byte] =
    fill(
      scale(previous.length)
    )(
      0.toByte
    )

  def apply(in: Array[Byte]): Array[Byte] =
    apply(
      in,
      scaleBuffer(in)
    )

  def apply(in: Array[Byte], out: Array[Byte], attempt: Int = 1): Array[Byte] = {
    val inflater = new java.util.zip.Inflater()
    inflater.setInput(in)
    val n = inflater.inflate(out)
    if (n == 0 || n == out.length)
      if (attempt == 3 || out.length == Int.MaxValue) {
        throw new IllegalStateException(
          s"Failed to inflate ${in.length} bytes ${attempt}x (final attempt given ${out.length} bytes; return code $n)"
        )
      } else
        apply(
          in,
          scaleBuffer(out),
          attempt + 1
        )
    else
      out
  }
}
