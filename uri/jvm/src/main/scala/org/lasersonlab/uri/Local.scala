package org.lasersonlab.uri

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Files
import java.nio.file.Files.newDirectoryStream

import cats.effect.Sync
import cats.implicits._
import org.apache.commons.io.IOUtils

case class Local[F[_]: Sync](file: String)(
  implicit
  val config: Config
)
extends Uri[F] {
  val f = new File(file).getCanonicalFile
  val path = f.toPath
  val  uri = f.toURI

  type Self = Local[F]

  override def /(name: String): Local[F] = Local(s"$file/$name")

  override def exists: F[Boolean] = delay { Files.exists(path) }

  import scala.collection.JavaConverters._
  override def list: F[List[Local[F]]] =
    delay {
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

  override def parentOpt: Option[Local[F]] = Option(f.getParent).map(Local[F](_))

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

object Local {

}
