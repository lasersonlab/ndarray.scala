package com.tom_e_white.hdf5_experiments;

import ucar.unidata.io.RandomAccessFile;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

public class NioReadOnlyRandomAccessFile extends RandomAccessFile {

    private static final int DEFAULT_BUFFER_SIZE = 64 * 1024;

    private final Path path;
    private final SeekableByteChannel sbc;

    public NioReadOnlyRandomAccessFile(Path path) throws IOException {
        this(path, DEFAULT_BUFFER_SIZE);
    }

    public NioReadOnlyRandomAccessFile(Path path, int bufferSize) throws IOException {
        super(bufferSize);
        this.path = path;
        this.sbc = Files.newByteChannel(path);
    }

    @Override
    protected int read_(final long pos, final byte[] buffer, final int offset, final int length) throws IOException {
        if (length < 0) {
            throw new IndexOutOfBoundsException();
        }
        sbc.position(pos);
        ByteBuffer buf = ByteBuffer.wrap(buffer, offset, length);
        int n = 0;
        while (n < length) {
            final int count = sbc.read(buf);
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
        try {
            return Files.getLastModifiedTime(path).to(TimeUnit.MILLISECONDS);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
