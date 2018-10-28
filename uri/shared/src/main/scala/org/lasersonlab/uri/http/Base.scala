package org.lasersonlab.uri.http

import java.net.URI

import cats.implicits._
import org.lasersonlab.uri._

import scala.util.Try

trait Base[F[_]] {
  self: Uri[F] ⇒

  type Self <: Uri[F]

  def make(uri: URI): Self

  override def exists: F[Boolean] = sizeOpt.map { _.isSuccess }

  override def parentOpt: Option[Self] = uri.parentOpt.map(make)

  override def size: F[Long] = sizeOpt.map { _.toEither }.rethrow

  def sizeOpt: F[Try[Long]]
}
