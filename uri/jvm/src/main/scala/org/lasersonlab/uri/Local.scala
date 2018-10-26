package org.lasersonlab.uri

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Files

import cats.effect.Sync
import cats.implicits._
import org.apache.commons.io.IOUtils

case class Local[F[_]](file: File)(
  implicit
  val F: Sync[F],
  val config: Config
)
extends Uri[F] {
  val path = file.toPath
  val uri = file.toURI

  import F.delay

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    delay {
      val ch = Files.newByteChannel(path)
      ch.position(start)
      val buffer = ByteBuffer.allocate(size)
      IOUtils.readFully(ch, buffer)
      buffer.array()
    }

  override def size: F[Long] = delay { new File(uri).length() }
}
