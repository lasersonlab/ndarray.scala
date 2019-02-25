package org.lasersonlab.uri

import java.io.OutputStream
import java.net.URI

import org.lasersonlab.uri.Local.{ fs, path }

import scala.concurrent.ExecutionContext
import scala.scalajs.js.Dictionary
import scala.scalajs.js.typedarray.Int8Array

case class Local(str: String)(
  implicit
  val cachingConfig: caching.Config,
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

  def isDirectory: Boolean = stat.isDirectory().asInstanceOf[Boolean]

  override def children: F[Iterator[Local]] = F { childrenSync }
  def childrenSync: Iterator[Local] =
    if (!existsSync || !isDirectory)
      Iterator()
    else {
      val result =
      fs
        .readdirSync(str)

      result
        .asInstanceOf[scalajs.js.Array[String]]
        .toArray
        .iterator
        .map(base ⇒ Local(s"$str/$base"))
    }

  override def outputStream: OutputStream = {
    val parent = this.parent

    if (!parent.existsSync) {
      // Doesn't work properly on Node <10! https://stackoverflow.com/a/40686853/544236
      fs.mkdirSync(
        parent.str,
        Dictionary("recursive" → true)
      )
    }

    new OutputStream {
      val fd =
        fs.openSync(
          str,
          "w"
        )

      import scala.scalajs.js.typedarray._

      override def write(b: Int): Unit = write(Array(b.toByte))
      override def write(b: Array[Byte]): Unit = fs.writeSync(fd, b.toTypedArray)
      override def write(b: Array[Byte], off: Int, len: Int): Unit = write(b.slice(off, off + len))
      override def flush(): Unit = super.flush()
      override def close(): Unit = fs.closeSync(fd)
    }
  }

  override def delete: F[Unit] = F { deleteSync() }
  def deleteSync(): Unit =
    if (isDirectory)
      fs.rmdirSync(str)
    else
      fs.unlinkSync(str)
  def deleteSync(recursive: Boolean): Unit = {
    if (recursive && isDirectory)
      childrenSync
        .foreach {
          _.deleteSync(recursive = true)
        }

    deleteSync()
  }

  override def parentOpt: Option[Self] =
    if (!existsSync) {
      val parent = path.dirname(path.normalize(str)).asInstanceOf[String]
      if (parent == str)
        None
      else
        Some(Local(parent))
    } else
      fs
        .realpathSync(str)
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
  val path = g.require("path")
  def cwd(implicit ec: ExecutionContext) = Local(g.process.cwd().asInstanceOf[String])
}
