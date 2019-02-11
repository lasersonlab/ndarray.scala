package org.lasersonlab.commons

import java.io.{ ByteArrayOutputStream, IOException, InputStream, OutputStream }

import cats.implicits._
import hammerlab.math.utils._

object IOUtils {

  val EOF = -1
  val DEFAULT_BUFFER_SIZE = 1024 * 4

  /**
   * Gets the contents of an <code>InputStream</code> as a <code>byte[]</code>.
   * <p>
   * This method buffers the input internally, so there is no need to use a
   * <code>BufferedInputStream</code>.
   *
   * @param input the <code>InputStream</code> to read from
   * @return the requested byte array
   * @throws NullPointerException if the input is null
   * @throws IOException          if an I/O error occurs
   */
  @throws[IOException]
  def toByteArray(input: InputStream): Array[Byte] = {
    val output = new ByteArrayOutputStream
    copy(input, output)
    output.toByteArray
  }

  /**
   * Gets contents of an <code>InputStream</code> as a <code>byte[]</code>.
   * Use this method instead of <code>toByteArray(InputStream)</code>
   * when <code>InputStream</code> size is known.
   * <b>NOTE:</b> the method checks that the length can safely be cast to an int without truncation
   * before using [[IOUtils.toByteArray]] to read into the byte array. (Arrays can have no more than Integer.MAX_VALUE
   * entries anyway)
   *
   * @param input the <code>InputStream</code> to read from
   * @param size  the size of <code>InputStream</code>
   * @return the requested byte array
   * @throws IOException              if an I/O error occurs or <code>InputStream</code> size differ from parameter
   *                                  size
   * @throws IllegalArgumentException if size is less than zero or size is greater than Integer.MAX_VALUE
   * @see IOUtils#toByteArray(java.io.InputStream, int)
   * @since 2.1
   */
  @throws[IOException]
  def toByteArray(input: InputStream, size: Long): Array[Byte] =
    toByteArray(
      input,
      size
        .int_!(
          "Size cannot be greater than Integer max value: " + size,
        )
    )

  /**
   * Gets the contents of an <code>InputStream</code> as a <code>byte[]</code>.
   * Use this method instead of <code>toByteArray(InputStream)</code>
   * when <code>InputStream</code> size is known
   *
   * @param input the <code>InputStream</code> to read from
   * @param size  the size of <code>InputStream</code>
   * @return the requested byte array
   * @throws IOException              if an I/O error occurs or <code>InputStream</code> size differ from parameter
   *                                  size
   * @throws IllegalArgumentException if size is less than zero
   * @since 2.1
   */
  @throws[IOException]
  def toByteArray(input: InputStream, size: Int): Array[Byte] = {
    if (size < 0) throw new IllegalArgumentException("Size must be equal or greater than zero: " + size)
    if (size == 0) return new Array[Byte](0)
    val data = new Array[Byte](size)
    var offset = 0
    var readed = 0
    while (
      offset < size &&
        {
          readed = input.read(data, offset, size - offset)
          readed != EOF
        }
    ) offset += readed
    if (offset != size) throw new IOException("Unexpected readed size. current: " + offset + ", excepted: " + size)
    data
  }

  /**
   * Copies bytes from an <code>InputStream</code> to an
   * <code>OutputStream</code>.
   * <p>
   * This method buffers the input internally, so there is no need to use a
   * <code>BufferedInputStream</code>.
   * <p>
   * Large streams (over 2GB) will return a bytes copied value of
   * <code>-1</code> after the copy has completed since the correct
   * number of bytes cannot be returned as an int. For large streams
   * use the <code>copyLarge(InputStream, OutputStream)</code> method.
   *
   * @param input  the <code>InputStream</code> to read from
   * @param output the <code>OutputStream</code> to write to
   * @return the number of bytes copied, or -1 if &gt; Integer.MAX_VALUE
   * @throws NullPointerException if the input or output is null
   * @throws IOException          if an I/O error occurs
   * @since 1.1
   */
  @throws[IOException]
  def copy(input: InputStream, output: OutputStream): Int =
    copyLarge(
      input,
      output
    )
    .safeInt
    .getOrElse(-1)

  /**
   * Copies bytes from an <code>InputStream</code> to an <code>OutputStream</code> using an internal buffer of the
   * given size.
   * <p>
   * This method buffers the input internally, so there is no need to use a <code>BufferedInputStream</code>.
   * <p>
   *
   * @param input      the <code>InputStream</code> to read from
   * @param output     the <code>OutputStream</code> to write to
   * @param bufferSize the bufferSize used to copy from the input to the output
   * @return the number of bytes copied
   * @throws NullPointerException if the input or output is null
   * @throws IOException          if an I/O error occurs
   * @since 2.5
   */
  @throws[IOException]
  def copy(input: InputStream, output: OutputStream, bufferSize: Int): Long = copyLarge(input, output, new Array[Byte](bufferSize))

  /**
   * Copies bytes from a large (over 2GB) <code>InputStream</code> to an
   * <code>OutputStream</code>.
   * <p>
   * This method buffers the input internally, so there is no need to use a
   * <code>BufferedInputStream</code>.
   * <p>
   * The buffer size is given by {@link #DEFAULT_BUFFER_SIZE}.
   *
   * @param input  the <code>InputStream</code> to read from
   * @param output the <code>OutputStream</code> to write to
   * @return the number of bytes copied
   * @throws NullPointerException if the input or output is null
   * @throws IOException          if an I/O error occurs
   * @since 1.3
   */
  @throws[IOException]
  def copyLarge(input: InputStream, output: OutputStream): Long = copy(input, output, DEFAULT_BUFFER_SIZE)

  /**
   * Copies bytes from a large (over 2GB) <code>InputStream</code> to an
   * <code>OutputStream</code>.
   * <p>
   * This method uses the provided buffer, so there is no need to use a
   * <code>BufferedInputStream</code>.
   * <p>
   *
   * @param input  the <code>InputStream</code> to read from
   * @param output the <code>OutputStream</code> to write to
   * @param buffer the buffer to use for the copy
   * @return the number of bytes copied
   * @throws NullPointerException if the input or output is null
   * @throws IOException          if an I/O error occurs
   * @since 2.2
   */
  @throws[IOException]
  def copyLarge(input: InputStream, output: OutputStream, buffer: Array[Byte]): Long = {
    var count = 0
    var n = 0
    while ({
      n = input.read(buffer)
      EOF != n
    }) {
      output.write(buffer, 0, n)
      count += n
    }
    count
  }

}
