package org.lasersonlab.zarr.cmp.untyped

import org.hammerlab.test.Cmp
import org.lasersonlab.zarr.cmp.json
import org.lasersonlab.zarr.untyped
import org.lasersonlab.zarr.untyped.Group
import shapeless.the

object group {
//  sealed trait FieldDiff
//  object FieldDiff {
//    case class  LeftOnly(name: String            ) extends FieldDiff
//    case class RightOnly(name: String            ) extends FieldDiff
//    case class  Mismatch(name: String, diff: FieldDiff) extends FieldDiff
//  }
//  case class Diff(
//    groups:    Seq[      Diff],
//    arrays:    Seq[array.cmp.arrayCmp.Diff],
//     attrs: Option[ json.Diff]
//  )

  //import cmp.groupCmp
//  import array.cmp.arrayCmp

//  the[Cmp[Map[String, untyped.Array]]]
//  the[Cmp[Group]]

  trait cmp {
//    implicit val groupCmp: Cmp[Group] =
//      Cmp.by {
//        group ⇒
//          (
//            group.arrays,
//            group.groups,
//            group.attrs,
//            group.metadata
//          )
//      }
//      new Cmp[Group] {
//        type Diff = group.Diff
//        def cmp(l: Group, r: Group): Option[Diff] = {
//          ???
//        }
//      }
  }
  object cmp extends cmp
}
