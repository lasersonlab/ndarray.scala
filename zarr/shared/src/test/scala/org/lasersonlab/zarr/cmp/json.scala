package org.lasersonlab.zarr.cmp

import io.circe.Json
import org.hammerlab.cmp.CanEq.dsl
import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.cmp.json.Diff.{ DifferentTypes, Primitives }

object json {
  sealed trait Diff
  object Diff {
    case class DifferentTypes(l: Json, r: Json)       extends Diff
    case class  Primitives[D](l: Json, r: Json, d: D) extends Diff

    sealed trait IndexDiff
    object IndexDiff {
      case class     Left(json: Json) extends IndexDiff
      case class    Right(json: Json) extends IndexDiff
      case class Mismatch(diff: Diff) extends IndexDiff
    }
    case class Arrays(idx: Int, diff: IndexDiff) extends Diff

    case class UnmatchedField(key: String, value: Json)
    case class Objects(
           lefts: Seq[UnmatchedField],
          rights: Seq[UnmatchedField],
      mismatches: Seq[          Diff]
    )
    extends Diff
  }

  trait cmp {
//    implicit def jsonCmp(
//      implicit
//      double: Cmp[Double],
//      strings: Cmp[String]
//    ):
//          Cmp[Json] =
//      new Cmp[Json] {
//        type Diff = json.Diff
//        def cmp(l: Json, r: Json): Option[Diff] =
//          (l.asBoolean, r.asBoolean) match {
//            case (Some(lb), Some(rb)) ⇒ dsl.cmp(lb, rb).map(Primitives(l, r, _))
//            case (None, None) ⇒ ???
//            case _ ⇒ Some(DifferentTypes(l, r))
//          }
//      }
  }
  object cmp extends cmp
}
