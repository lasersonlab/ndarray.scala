package org.lasersonlab.uri.http

import java.net.URI

import cats.implicits._
import org.lasersonlab.uri._

import scala.util.Try

trait Base {
  self: Uri ⇒

  def make(uri: URI): Self

  override def exists: F[Boolean] = sizeOpt.map { _.isSuccess }

  override def parentOpt: Option[Self] = uri.parentOpt.map(make)

  override lazy val size: F[Long] = sizeOpt.map { _.toEither }.rethrow

  def sizeOpt: F[Try[Long]]

  override def /(name: String): Self = make(uri.resolve(name))

  override def children: F[Iterator[Self]] = throw new UnsupportedOperationException(s"Can't list HTTP URIs")

  override def isDirectory: Boolean = uri.toString.endsWith("/")
}
