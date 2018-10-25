package org.lasersonlab.zarr.cmp.untyped

import cats.data.NonEmptyList
import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.untyped.Struct

object map {
  sealed trait Diff
  case class     Left(key: String                ) extends Diff
  case class    Right(key: String                ) extends Diff
  case class Mismatch(key: String, l: Any, r: Any) extends Diff

  implicit object cmpMapStringAny
    extends Cmp[Map[String, Any]] {
    type Diff = NonEmptyList[map.Diff]
    def cmp(l: Map[String, Any], r: Map[String, Any]): Option[Diff] = {
      val lk = l.keySet
      val rk = r.keySet
      val  lefts = lk.diff(rk).map(Left)
      val rights = rk.diff(lk).map(Right)
      val mismatches =
        for {
          (k, l) ← l
          r ← r.get(k)
          if l != r
        } yield
          Mismatch(k, l, r)

      NonEmptyList.fromList(
        lefts ++
        rights ++
        mismatches
        toList
      )
    }
  }
}

trait struct
  extends Cmp[Struct] {
  type Diff = NonEmptyList[map.Diff]
  def cmp(l: Struct, r: Struct): Option[Diff] = map.cmpMapStringAny(l.values, r.values)
  implicit val cmpStruct: Cmp[Struct] = this
}
