package com.tom_e_white.hdf5_java_cloud;

import org.hammerlab.channel.CachingChannel;
import org.hammerlab.channel.CachingChannel$;
import org.hammerlab.channel.SeekableByteChannel$;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import ucar.unidata.io.RandomAccessFile;

import java.io.IOException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

/**
 * A read-only <a href="https://docs.oracle.com/javase/7/docs/api/java/nio/package-summary.html">Java NIO</a>-based
 * implementation of
 * <a href="https://www.unidata.ucar.edu/software/thredds/current/netcdf-java/documentation.htm">netCDF</a>'s
 * {@link RandomAccessFile} interface (which is different to the JDK one).
 * In particular, this allows Java programs to read HDF5 files from cloud object stores using the appropriate NIO
 * implementation.
 */
public class NioReadOnlyRandomAccessFile extends RandomAccessFile {

  private static final Logger logger = LoggerFactory.getLogger(NioReadOnlyRandomAccessFile.class);

  private static final int DEFAULT_BUFFER_SIZE = 32 * 1024;

  private final Path path;
  private final CachingChannel<org.hammerlab.channel.SeekableByteChannel.ChannelByteChannel> cachingChannel;
  private final long lastModified;

  public NioReadOnlyRandomAccessFile(URI uri) throws IOException {
    this(Paths.get(uri));
  }

  public NioReadOnlyRandomAccessFile(Path path) throws IOException {
    this(path, DEFAULT_BUFFER_SIZE);
  }

  public NioReadOnlyRandomAccessFile(Path path, int bufferSize) throws IOException {
    super(bufferSize);
    this.path = path;
    SeekableByteChannel sbc = Files.newByteChannel(path);
    CachingChannel.Config config = new CachingChannel.Config(1 << 20, 2, 1 << 28);
    this.cachingChannel = CachingChannel$.MODULE$.makeCachingChannel(SeekableByteChannel$.MODULE$.makeChannelByteChannel(sbc), config);
    this.lastModified = Files.getLastModifiedTime(path).to(TimeUnit.MILLISECONDS);
  }

  @Override
  protected int read_(final long pos, final byte[] buffer, final int offset, final int length) throws IOException {
    if (length < 0) {
      throw new IndexOutOfBoundsException();
    }
    if (logger.isDebugEnabled()) {
      logger.debug("Calling read at pos {} for length {}", pos, length);
    }

    cachingChannel.seek(pos);
    ByteBuffer buf = ByteBuffer.wrap(buffer, offset, length);
    int n = 0;
    while (n < length) {
      final int count = cachingChannel.read(buf);
      if (count < 0) {
        if (n > 0) {
          return n;
        } else {
          return count;
        }
      }
      n += count;
    }
    return n;
  }

  @Override
  public long readToByteChannel(WritableByteChannel dest, long offset, long nbytes) throws IOException {
    int n = (int) nbytes;
    byte[] buff = new byte[n];
    int done = read_(offset, buff, 0, n);
    dest.write(ByteBuffer.wrap(buff));
    return done;
  }

  @Override
  public long length() throws IOException {
    return Files.size(path);
  }

  @Override
  public long getLastModified() {
    return lastModified;
  }

  @Override
  public String toString() {
    return path.toString();
  }
}
