package org.lasersonlab.uri

import java.io.IOException
import java.net.URI

import cats.effect.Sync

import scala.collection.generic.CanBuildFrom
import scala.scalajs.js.typedarray.Int8Array

case class Local[F[_]: Sync](file: String)(
  implicit
  val config: Config
)
extends Uri[F] {

  type Self = Local[F]

  import scalajs.js.Dynamic.{ global ⇒ g }
  val fs = g.require("fs")

  override def /(name: String): Local[F] = Local(s"$file/$name")

  override def exists: F[Boolean] =
    delay {
      fs
        .existsSync(file)
        .asInstanceOf[Boolean]
    }

  implicit def cbfList[From, To]: CanBuildFrom[From, To, List[To]] = ???

//  import hammerlab.collection._
  override def list: F[List[Local[F]]] =
    delay {
      fs
        .readdirSync
        .asInstanceOf[Array[String]]
        .map[
          Local[F],
          List[Local[F]]
        ](
          Local(_)
        )
    }

  override def parentOpt: Option[Local[F]] =
    fs
      .realPathSync(file)
      .asInstanceOf[String]
      .split("/")
      .toVector match {
      case Vector() ⇒ None
      case parent :+ _ ⇒ Some(Local(parent.mkString("/")))
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
