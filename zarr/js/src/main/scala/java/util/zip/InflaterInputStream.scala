/*
 * Copyright (c) 1996, 2013, Oracle and/or its affiliates. All rights reserved.
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package java.util.zip

import java.io.FilterInputStream
import java.io.InputStream
import java.io.IOException
import java.io.EOFException


/**
 * This class implements a stream filter for uncompressing data in the
 * "deflate" compression format. It is also used as the basis for other
 * decompression filters, such as GZIPInputStream.
 *
 * @see Inflater
 * @author David Connelly
 */
class InflaterInputStream(
  in: InputStream,

  /**
   * Decompressor for this stream.
   */
  var inf: Inflater, val size: Int
)
/**
 * Creates a new input stream with the specified decompressor and
 * buffer size.
 *
 * @param in   the input stream
 * @param inf  the decompressor ("inflater")
 * @param size the input buffer size
 * @exception IllegalArgumentException if { @code size <= 0}
 */
  extends FilterInputStream(in) {
  if (in == null || inf == null) throw new NullPointerException
  else if (size <= 0) throw new IllegalArgumentException("buffer size <= 0")
  buf = new Array[Byte](size)
  /**
   * Input buffer for decompression.
   */
  protected var buf: Array[Byte] = null
  /**
   * Length of input buffer.
   */
  protected var len = 0
  private var closed = false
  // this flag is set to true after EOF has reached
  private var reachEOF = false

  /**
   * Check to make sure that this stream has not been closed
   */
  @throws[IOException]
  private def ensureOpen(): Unit = {
    if (closed) throw new IOException("Stream closed")
  }

  /**
   * Creates a new input stream with the specified decompressor and a
   * default buffer size.
   *
   * @param in  the input stream
   * @param inf the decompressor ("inflater")
   */
  def this(in: InputStream, inf: Inflater) {
    this(in, inf, 512)
  }

  private[zip] var usesDefaultInflater = false

  /**
   * Creates a new input stream with a default decompressor and buffer size.
   *
   * @param in the input stream
   */
  def this(in: InputStream) {
    this(in, new Inflater)
    usesDefaultInflater = true
  }

  private val singleByteBuf = new Array[Byte](1)

  /**
   * Reads a byte of uncompressed data. This method will block until
   * enough input is available for decompression.
   *
   * @return the byte read, or -1 if end of compressed input is reached
   * @exception IOException if an I/O error has occurred
   */
  @throws[IOException]
  override def read: Int = {
    ensureOpen()
    if (read(singleByteBuf, 0, 1) == -1) -1
    else java.lang.Byte.toUnsignedInt(singleByteBuf(0))
  }

  /**
   * Reads uncompressed data into an array of bytes. If <code>len</code> is not
   * zero, the method will block until some input can be decompressed; otherwise,
   * no bytes are read and <code>0</code> is returned.
   *
   * @param b   the buffer into which the data is read
   * @param off the start offset in the destination array <code>b</code>
   * @param len the maximum number of bytes read
   * @return the actual number of bytes read, or -1 if the end of the
   *         compressed input is reached or a preset dictionary is needed
   * @exception NullPointerException If <code>b</code> is <code>null</code>.
   * @exception IndexOutOfBoundsException If <code>off</code> is negative,
   *            <code>len</code> is negative, or <code>len</code> is greater than
   *            <code>b.length - off</code>
   * @exception ZipException if a ZIP format error has occurred
   * @exception IOException if an I/O error has occurred
   */
  @throws[IOException]
  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    ensureOpen()
    if (b == null) throw new NullPointerException
    else if (off < 0 || len < 0 || len > b.length - off) throw new IndexOutOfBoundsException
    else if (len == 0) return 0
    try {
      var n = inf.inflate(b, off, len)
      while (n == 0) {
        if (inf.finished || inf.needsDictionary) {
          reachEOF = true
          return -1
        }
        if (inf.needsInput) fill()
        n = inf.inflate(b, off, len)
      }
      n
    } catch {
      case e: DataFormatException ⇒
        val s = e.getMessage
        throw new ZipException(if (s != null) s
        else "Invalid ZLIB data format")
    }
  }

  /**
   * Returns 0 after EOF has been reached, otherwise always return 1.
   * <p>
   * Programs should not count on this method to return the actual number
   * of bytes that could be read without blocking.
   *
   * @return 1 before EOF and 0 after EOF.
   * @exception IOException  if an I/O error occurs.
   *
   */
  @throws[IOException]
  override def available: Int = {
    ensureOpen()
    if (reachEOF) 0
    else 1
  }

  private val b = new Array[Byte](512)

  /**
   * Skips specified number of bytes of uncompressed data.
   *
   * @param n the number of bytes to skip
   * @return the actual number of bytes skipped.
   * @exception IOException if an I/O error has occurred
   * @exception IllegalArgumentException if { @code n < 0}
   */
  @throws[IOException]
  override def skip(n: Long): Long = {
    if (n < 0) throw new IllegalArgumentException("negative skip length")
    ensureOpen()
    val max = Math.min(n, Integer.MAX_VALUE).toInt
    var total = 0
    var continue = true
    while (total < max && continue) {
      var len = max - total
      if (len > b.length) len = b.length
      len = read(b, 0, len)
      if (len == -1) {
        reachEOF = true
        continue = false
      } else
        total += len
    }
    total
  }

  /**
   * Closes this input stream and releases any system resources associated
   * with the stream.
   *
   * @exception IOException if an I/O error has occurred
   */
  @throws[IOException]
  override def close(): Unit = {
    if (!closed) {
      if (usesDefaultInflater) inf.end()
      in.close()
      closed = true
    }
  }

  /**
   * Fills input buffer with more data to decompress.
   *
   * @exception IOException if an I/O error has occurred
   */
  @throws[IOException]
  protected def fill(): Unit = {
    ensureOpen()
    len = in.read(buf, 0, buf.length)
    if (len == -1) throw new EOFException("Unexpected end of ZLIB input stream")
    inf.setInput(buf, 0, len)
  }

  /**
   * Tests if this input stream supports the <code>mark</code> and
   * <code>reset</code> methods. The <code>markSupported</code>
   * method of <code>InflaterInputStream</code> returns
   * <code>false</code>.
   *
   * @return a <code>boolean</code> indicating if this stream type supports
   *         the <code>mark</code> and <code>reset</code> methods.
   * @see java.io.InputStream#mark(int)
   * @see java.io.InputStream#reset()
   */
  override def markSupported = false

  /**
   * Marks the current position in this input stream.
   *
   * <p> The <code>mark</code> method of <code>InflaterInputStream</code>
   * does nothing.
   *
   * @param   readlimit the maximum limit of bytes that can be read before
   *                    the mark position becomes invalid.
   * @see java.io.InputStream#reset()
   */
  override def mark(readlimit: Int): Unit = {}

  /**
   * Repositions this stream to the position at the time the
   * <code>mark</code> method was last called on this input stream.
   *
   * <p> The method <code>reset</code> for class
   * <code>InflaterInputStream</code> does nothing except throw an
   * <code>IOException</code>.
   *
   * @exception IOException  if this method is invoked.
   * @see java.io.InputStream#mark(int)
   * @see java.io.IOException
   */
  @throws[IOException]
  override def reset(): Unit = {
    throw new IOException("mark/reset not supported")
  }
}
