package org.lasersonlab.zarr.dtype

import org.lasersonlab.zarr.dtype.DataType._
import shapeless._

/**
 * Auto-derivations of [[Struct]] instances for case-classes
 */
trait StructDerivations {

  implicit val hnil: StructList[HNil] =
    StructList[HNil](
      entries = Nil,
      size = 0
    )(
          _  ⇒ HNil,
      (_, _) ⇒ ()
    )

  implicit def cons[
    Head,
    Tail <: HList
  ](
    implicit
    head: Aux[Head],
    tail: StructList[Tail],
  ):
    StructList[Head :: Tail]
  = {
    implicit val headEntry =
      StructEntry(
        head.toString,
        head
      )

    StructList(
      headEntry :: tail.entries,
      head.size  + tail.size
    )(
      buff ⇒
        head(buff) ::
        tail(buff),
      {
        case (buffer, h :: t) ⇒
          head(buffer, h)
          tail(buffer, t)
      }
    )
  }

  implicit def struct[
    S,
    L <: HList
  ](
    implicit
    g: Generic.Aux[S, L],
    l: StructList[L]
  ):
    Aux[S] =
    Struct[S, L](l)
}
