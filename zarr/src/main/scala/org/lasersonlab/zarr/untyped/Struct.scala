package org.lasersonlab.zarr.untyped

import scala.collection.immutable.ListMap  // preserves insertion order; important for serialization

case class Struct(values: ListMap[String, Any]) {
  def apply[T](k: String): T =
    values(k)
      .asInstanceOf[T]

  def get[T](k: String): Option[T] =
    values
      .get(k)
      .map(
        _.asInstanceOf[T]
      )
}

object Struct {
  def apply(values: (String, Any)*): Struct =
    Struct(
      ListMap(
        values: _*
      )
    )

  implicit def unwrap(t: Struct): ListMap[String, Any] = t.values
}
