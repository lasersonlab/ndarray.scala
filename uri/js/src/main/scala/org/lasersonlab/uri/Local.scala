package org.lasersonlab.uri

import java.net.URI

import cats.FlatMap

case class Local[F[_]: FlatMap](uri: URI) extends Uri[F] {
  override def size: F[Long] = ???

  override val config: Config = _

  override def bytes(start: Long, size: Int): F[Array[Byte]] = ???
}
