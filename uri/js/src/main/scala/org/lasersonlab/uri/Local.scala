package org.lasersonlab.uri

import java.io.IOException
import java.net.URI

import Local.fs

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.scalajs.js.typedarray.Int8Array

case class Local(file: String)(
  implicit
  val config: Config,
  override val ec: ExecutionContext
)
extends Uri {

  type Self = Local

  override def /(name: String): Self = Local(s"$file/$name")

  override def exists: F[Boolean] =
    F {
      fs
        .existsSync(file)
        .asInstanceOf[Boolean]
    }

  implicit def cbfList[From, To]: CanBuildFrom[From, To, List[To]] =
    new CanBuildFrom[From, To, List[To]] {
      def apply(from: From): mutable.Builder[To, List[To]] = List.newBuilder
      def apply(): mutable.Builder[To, List[To]] = List.newBuilder
    }

  override def list: F[List[Self]] =
    F {
      fs
        .readdirSync
        .asInstanceOf[Array[String]]
        .map[
          Self,
          List[Self]
        ](
          Local(_)
        )
    }

  override def parentOpt: Option[Self] =
    fs
      .realPathSync(file)
      .asInstanceOf[String]
      .split("/")
      .toVector match {
      case Vector() ⇒ None
      case parent :+ _ ⇒ Some(Local(parent.mkString("/")))
    }

  lazy val stat = fs.statSync(file)

  override lazy val size =
    F {
      stat
        .size
        .asInstanceOf[Double]
        .toLong
    }

  override val uri: URI = new URI(file)

  override def bytes(start: Long, size: Int): F[Array[Byte]] =
    F {
      val fd = fs.openSync(file, "r")
      val arr = new Int8Array(size)
      val bytesRead = fs.readSync(fd, arr, 0, size, 0).asInstanceOf[Int]
//      if (bytesRead < size)
//        throw new IOException(
//          s"Expected $size bytes from $file, read $bytesRead"
//        )
//
      arr.take(bytesRead).toArray
    }
}

object Local {
  import scalajs.js.Dynamic.{ global ⇒ g }
  val fs = g.require("fs")
}
