package org.lasersonlab.uri

import java.net.URI

import org.lasersonlab.uri.Local.fs

import scala.concurrent.ExecutionContext
import scala.scalajs.js.typedarray.Int8Array

case class Local(str: String)(
  implicit
  val config: Config,
  override val ec: ExecutionContext
)
extends Uri {
  type Self = Local

  override def /(name: String): Self = Local(s"$str/$name")

  override def exists: F[Boolean] = F { existsSync }
  def existsSync: Boolean =
    fs
      .existsSync(str)
      .asInstanceOf[Boolean]

  override def list: F[Iterator[Local]] = F { listSync }
  def listSync: Iterator[Local] =
    fs
      .readdirSync
      .asInstanceOf[Array[String]]
      .iterator
      .map(Local(_))

  override def delete: F[Unit] = F { fs.deleteSync(str) }

  override def parentOpt: Option[Self] =
    fs
      .realPathSync(str)
      .asInstanceOf[String]
      .split("/")
      .toVector match {
      case Vector() ⇒ None
      case parent :+ _ ⇒ Some(Local(parent.mkString("/")))
    }

  lazy val stat = fs.statSync(str)

  override lazy val size =
    F {
      stat
        .size
        .asInstanceOf[Double]
        .toLong
    }

  override val uri: URI = new URI(str)

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    F {
      val fd = fs.openSync(str, "r")
      val arr = new Int8Array(size)
      val bytesRead = fs.readSync(fd, arr, 0, size, 0).asInstanceOf[Int]
      arr.take(bytesRead).toArray
    }
}

object Local {
  import scalajs.js.Dynamic.{ global ⇒ g }
  val fs = g.require("fs")
  lazy val cwd = g.process.cwd().asInstanceOf[String]
}
