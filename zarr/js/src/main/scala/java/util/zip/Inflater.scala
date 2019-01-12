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

/**
 * This class provides support for general purpose decompression using the
 * popular ZLIB compression library. The ZLIB compression library was
 * initially developed as part of the PNG graphics standard and is not
 * protected by patents. It is fully described in the specifications at
 * the <a href="package-summary.html#package_description">java.util.zip
 * package description</a>.
 *
 * <p>The following code fragment demonstrates a trivial compression
 * and decompression of a string using <tt>Deflater</tt> and
 * <tt>Inflater</tt>.
 *
 * <blockquote><pre>
 * try {
 * // Encode a String into bytes
 * String inputString = "blahblahblah\u20AC\u20AC";
 * byte[] input = inputString.getBytes("UTF-8");
 *
 * // Compress the bytes
 * byte[] output = new byte[100];
 * Deflater compresser = new Deflater();
 *     compresser.setInput(input);
 *     compresser.finish();
 * int compressedDataLength = compresser.deflate(output);
 *
 * // Decompress the bytes
 * Inflater decompresser = new Inflater();
 *     decompresser.setInput(output, 0, compressedDataLength);
 * byte[] result = new byte[100];
 * int resultLength = decompresser.inflate(result);
 *     decompresser.end();
 *
 * // Decode the bytes into a String
 * String outputString = new String(result, 0, resultLength, "UTF-8");
 * } catch(java.io.UnsupportedEncodingException ex) {
 * // handle
 * } catch (java.util.zip.DataFormatException ex) {
 * // handle
 * }
 * </pre></blockquote>
 *
 * @see Deflater
 * @author David Connelly
 *
 */
object Inflater {
  val defaultBuf = new Array[Byte](0)

  private def initIDs(): Unit

  private def init(nowrap: Boolean)

  private def setDictionary(addr: Long, b: Array[Byte], off: Int, len: Int): Unit

  private def getAdler(addr: Long)

  private def reset(addr: Long): Unit

  private def end(addr: Long): Unit

  try /* Zip library is loaded from System.initializeSystemClass */ initIDs()

}

class Inflater(val nowrap: Boolean)

/**
 * Creates a new decompressor. If the parameter 'nowrap' is true then
 * the ZLIB header and checksum fields will not be used. This provides
 * compatibility with the compression format used by both GZIP and PKZIP.
 * <p>
 * Note: When using the 'nowrap' option it is also necessary to provide
 * an extra "dummy" byte as input. This is required by the ZLIB native
 * library in order to support certain optimizations.
 *
 * @param nowrap if true then support GZIP compatible compression
 */ {
  zsRef = new ZStreamRef(Inflater.init(nowrap))
  final private var zsRef = null
  private var buf = Inflater.defaultBuf
  private var off = 0
  private var len = 0
  private var finished = false
  private var needDict = false
  private var bytesRead = 0L
  private var bytesWritten = 0L

  /**
   * Creates a new decompressor.
   */
  def this {
    this(false)
  }

  /**
   * Sets input data for decompression. Should be called whenever
   * needsInput() returns true indicating that more input data is
   * required.
   *
   * @param b   the input data bytes
   * @param off the start offset of the input data
   * @param len the length of the input data
   * @see Inflater#needsInput
   */
  def setInput(b: Array[Byte], off: Int, len: Int): Unit = {
    if (b == null) throw new NullPointerException
    if (off < 0 || len < 0 || off > b.length - len) throw new ArrayIndexOutOfBoundsException
    zsRef synchronized this.buf = b
    this.off = off
    this.len = len

  }

  /**
   * Sets input data for decompression. Should be called whenever
   * needsInput() returns true indicating that more input data is
   * required.
   *
   * @param b the input data bytes
   * @see Inflater#needsInput
   */
  def setInput(b: Array[Byte]): Unit = {
    setInput(b, 0, b.length)
  }

  /**
   * Sets the preset dictionary to the given array of bytes. Should be
   * called when inflate() returns 0 and needsDictionary() returns true
   * indicating that a preset dictionary is required. The method getAdler()
   * can be used to get the Adler-32 value of the dictionary needed.
   *
   * @param b   the dictionary data bytes
   * @param off the start offset of the data
   * @param len the length of the data
   * @see Inflater#needsDictionary
   * @see Inflater#getAdler
   */
  def setDictionary(b: Array[Byte], off: Int, len: Int): Unit = {
    if (b == null) throw new NullPointerException
    if (off < 0 || len < 0 || off > b.length - len) throw new ArrayIndexOutOfBoundsException
    zsRef synchronized ensureOpen()
    Inflater.setDictionary(zsRef.address, b, off, len)
    needDict = false

  }

  /**
   * Sets the preset dictionary to the given array of bytes. Should be
   * called when inflate() returns 0 and needsDictionary() returns true
   * indicating that a preset dictionary is required. The method getAdler()
   * can be used to get the Adler-32 value of the dictionary needed.
   *
   * @param b the dictionary data bytes
   * @see Inflater#needsDictionary
   * @see Inflater#getAdler
   */
  def setDictionary(b: Array[Byte]): Unit = {
    setDictionary(b, 0, b.length)
  }

  /**
   * Returns the total number of bytes remaining in the input buffer.
   * This can be used to find out what bytes still remain in the input
   * buffer after decompression has finished.
   *
   * @return the total number of bytes remaining in the input buffer
   */
  def getRemaining: Int = {
    zsRef synchronized
      len

  }

  /**
   * Returns true if no data remains in the input buffer. This can
   * be used to determine if #setInput should be called in order
   * to provide more input.
   *
   * @return true if no data remains in the input buffer
   */
  def needsInput: Boolean = {
    zsRef synchronized
      len <= 0

  }

  /**
   * Returns true if a preset dictionary is needed for decompression.
   *
   * @return true if a preset dictionary is needed for decompression
   * @see Inflater#setDictionary
   */
  def needsDictionary: Boolean = {
    zsRef synchronized
      needDict

  }

  /**
   * Returns true if the end of the compressed data stream has been
   * reached.
   *
   * @return true if the end of the compressed data stream has been
   *         reached
   */
  def finished: Boolean = {
    zsRef synchronized
      finished

  }

  /**
   * Uncompresses bytes into specified buffer. Returns actual number
   * of bytes uncompressed. A return value of 0 indicates that
   * needsInput() or needsDictionary() should be called in order to
   * determine if more input data or a preset dictionary is required.
   * In the latter case, getAdler() can be used to get the Adler-32
   * value of the dictionary required.
   *
   * @param b   the buffer for the uncompressed data
   * @param off the start offset of the data
   * @param len the maximum number of uncompressed bytes
   * @return the actual number of uncompressed bytes
   * @exception DataFormatException if the compressed data format is invalid
   * @see Inflater#needsInput
   * @see Inflater#needsDictionary
   */
  @throws[DataFormatException]
  def inflate(b: Array[Byte], off: Int, len: Int): Int = {
    if (b == null) throw new NullPointerException
    if (off < 0 || len < 0 || off > b.length - len) throw new ArrayIndexOutOfBoundsException
    zsRef synchronized ensureOpen()
    val thisLen = this.len
    val n = inflateBytes(zsRef.address, b, off, len)
    bytesWritten += n
    bytesRead += (thisLen - this.len)
    n

  }

  /**
   * Uncompresses bytes into specified buffer. Returns actual number
   * of bytes uncompressed. A return value of 0 indicates that
   * needsInput() or needsDictionary() should be called in order to
   * determine if more input data or a preset dictionary is required.
   * In the latter case, getAdler() can be used to get the Adler-32
   * value of the dictionary required.
   *
   * @param b the buffer for the uncompressed data
   * @return the actual number of uncompressed bytes
   * @exception DataFormatException if the compressed data format is invalid
   * @see Inflater#needsInput
   * @see Inflater#needsDictionary
   */
  @throws[DataFormatException]
  def inflate(b: Array[Byte]): Int = inflate(b, 0, b.length)

  /**
   * Returns the ADLER-32 value of the uncompressed data.
   *
   * @return the ADLER-32 value of the uncompressed data
   */
  def getAdler: Int = {
    zsRef synchronized ensureOpen()
    Inflater.getAdler(zsRef.address)

  }

  /**
   * Returns the total number of compressed bytes input so far.
   *
   * <p>Since the number of bytes may be greater than
   * Integer.MAX_VALUE, the {@link #getBytesRead()} method is now
   * the preferred means of obtaining this information.</p>
   *
   * @return the total number of compressed bytes input so far
   */
  def getTotalIn: Int = getBytesRead.toInt

  /**
   * Returns the total number of compressed bytes input so far.
   *
   * @return the total (non-negative) number of compressed bytes input so far
   * @since 1.5
   */
  def getBytesRead: Long = {
    zsRef synchronized ensureOpen()
    bytesRead

  }

  /**
   * Returns the total number of uncompressed bytes output so far.
   *
   * <p>Since the number of bytes may be greater than
   * Integer.MAX_VALUE, the {@link #getBytesWritten()} method is now
   * the preferred means of obtaining this information.</p>
   *
   * @return the total number of uncompressed bytes output so far
   */
  def getTotalOut: Int = getBytesWritten.toInt

  /**
   * Returns the total number of uncompressed bytes output so far.
   *
   * @return the total (non-negative) number of uncompressed bytes output so far
   * @since 1.5
   */
  def getBytesWritten: Long = {
    zsRef synchronized ensureOpen()
    bytesWritten

  }

  /**
   * Resets inflater so that a new set of input data can be processed.
   */
  def reset(): Unit = {
    zsRef synchronized ensureOpen()
    Inflater.reset(zsRef.address)
    buf = Inflater.defaultBuf
    finished = false
    needDict = false
    off = len = 0
    bytesRead = bytesWritten = 0

  }

  /**
   * Closes the decompressor and discards any unprocessed input.
   * This method should be called when the decompressor is no longer
   * being used, but will also be called automatically by the finalize()
   * method. Once this method is called, the behavior of the Inflater
   * object is undefined.
   */
  def end(): Unit = {
    zsRef synchronized
    val addr = zsRef.address
    zsRef.clear()
    if (addr != 0) {
      Inflater.end(addr)
      buf = null
    }

  }

  /**
   * Closes the decompressor when garbage is collected.
   */
  override protected def finalize(): Unit = {
    end()
  }

  private def ensureOpen(): Unit = {
    assert(Thread.holdsLock(zsRef))
    if (zsRef.address == 0) throw new NullPointerException("Inflater has been closed")
  }

  private[zip] def ended = {
    zsRef synchronized
      zsRef.address == 0

  }

  @throws[DataFormatException]
  private def inflateBytes(addr: Long, b: Array[Byte], off: Int, len: Int)
}
