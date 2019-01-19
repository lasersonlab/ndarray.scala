package org.lasersonlab.uri

import java.io.{ File, FileOutputStream, OutputStream }
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
  val cachingConfig: caching.Config,
  override val ec: ExecutionContext
)
extends Uri {
  val path = file.toPath
  val  uri = file.toURI

  type Self = Local

  override def toString: String = file.toString

  override def /(name: String): Self = Local(new File(file, name))

  override def exists: F[Boolean] = F { existsSync }
  def existsSync: Boolean = Files.exists(path)

  override def children: F[Iterator[Local]] = F {childrenSync }
  def childrenSync: Iterator[Local] =
    newDirectoryStream(path)
      .asScala
      .map {
        path ⇒
          Local(path.toFile)
      }
      .iterator

  def isDirectory: Boolean = file.isDirectory

  override def delete: F[Unit] = F { deleteSync() }
  def deleteSync(): Unit = file.delete()
  def deleteSync(recursive: Boolean): Unit = {
    if (recursive && isDirectory)
      childrenSync
        .foreach {
          _.deleteSync(recursive = true)
        }

    deleteSync()
  }

  override def outputStream: OutputStream = {
    val parent = this.parent
    if (!parent.existsSync) {
      logger.info(s"Making intermediate directories: $parent")
      Files.createDirectories(parent.file.toPath)
    }
    assert(parent.existsSync)
    logger.info(s"Parent exists: $parent")

    new FileOutputStream(file)
  }

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
    cachingConfig: caching.Config,
    ec: ExecutionContext
  ): Local = new Local(file.getCanonicalFile)

  def apply(str: String)(
    implicit
    cachingConfig: caching.Config,
    ec: ExecutionContext
  ): Local = new Local(new File(str).getCanonicalFile)

  def cwd(implicit ec: ExecutionContext) = Local(new File("."))
}
