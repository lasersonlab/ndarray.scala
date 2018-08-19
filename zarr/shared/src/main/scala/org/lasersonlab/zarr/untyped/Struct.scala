package org.lasersonlab.zarr.untyped

case class Struct(values: Map[String, Any]) {
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
