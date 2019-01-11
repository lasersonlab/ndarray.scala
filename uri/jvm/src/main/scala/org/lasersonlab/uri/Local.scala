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

case class Local private(file: File)(
  implicit
  val config: Config,
  override val ec: ExecutionContext
)
extends Uri {
  val path = file.toPath
  val  uri = file.toURI

  type Self = Local

  override def /(name: String): Self = Local(new File(file, name))

  override def exists: F[Boolean] = F { existsSync }
  def existsSync: Boolean = Files.exists(path)

  override def list: F[Iterator[Local]] = F { listSync }
  def listSync: Iterator[Local] =
    newDirectoryStream(path)
      .asScala
      .map {
        path ⇒
          Local(path.toFile)
      }
      .iterator

  override def delete: F[Unit] = F { file.delete() }

  override def parentOpt: Option[Self] = Option(file.getParentFile).map(Local(_))

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

object Local {
  implicit def toFile(l: Local): File = l.file

  def apply(file: File)(
    implicit
    config: Config,
    ec: ExecutionContext
  ): Local = new Local(file.getCanonicalFile)

  def apply(str: String)(
    implicit
    config: Config,
    ec: ExecutionContext
  ): Local = new Local(new File(str).getCanonicalFile)

  lazy val cwd = new File(".").toString
}
