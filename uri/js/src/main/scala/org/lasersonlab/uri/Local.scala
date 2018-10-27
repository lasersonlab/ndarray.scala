package org.lasersonlab.uri

import java.io.IOException
import java.net.URI

import cats.effect.Sync

import scala.scalajs.js.typedarray.Int8Array

case class Local[F[_]: Sync](file: String)(
  implicit
  val config: Config
)
extends Uri[F] {

  import scalajs.js.Dynamic.{ global ⇒ g }
  val fs = g.require("fs")

  override def exists: F[Boolean] =
    delay {
      fs
        .existsSync(file)
        .asInstanceOf[Boolean]
    }

  lazy val stat = fs.statSync(file)
  lazy val size =
    delay {
      stat
        .size
        .asInstanceOf[Double]
        .toLong
    }

  override val uri: URI = new URI(file)

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    delay {
      val fd = fs.openSync(file, "r")
      val arr = new Int8Array(size)
      val bytesRead = fs.readSync(fd, arr, 0, size, 0).asInstanceOf[Int]
      if (bytesRead < size)
        throw new IOException(
          s"Expected $size bytes from $file, read $bytesRead"
        )

      arr.toArray
    }
}
