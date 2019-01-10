package org.lasersonlab.uri

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Files
import java.nio.file.Files.newDirectoryStream

import cats.implicits._
import org.apache.commons.io.IOUtils

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import math.min

case class Local(file: String)(
  implicit
  val config: Config,
  override val ec: ExecutionContext
)
extends Uri {
  val f = new File(file).getCanonicalFile
  val path = f.toPath
  val  uri = f.toURI

  type Self = Local

  override def /(name: String): Self = Local(s"$file/$name")

  override def exists: F[Boolean] = F { Files.exists(path) }

  override def list: F[List[Local]] =
    F {
      newDirectoryStream(path)
        .asScala
        .map {
          path ⇒
            Local(
              path
                .toFile
                .toString
            )
        }
        .toList
    }

  override def parentOpt: Option[Self] = Option(f.getParent).map(Local(_))

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    F {
      val ch = Files.newByteChannel(path)
      ch.position(start)
      val remaining = min(size, ch.size() - start toInt)
      val buffer = ByteBuffer.allocate(remaining)
      IOUtils.readFully(ch, buffer)
      buffer.array()
    }

  override lazy val size: F[Long] = F { new File(uri).length() }
}
