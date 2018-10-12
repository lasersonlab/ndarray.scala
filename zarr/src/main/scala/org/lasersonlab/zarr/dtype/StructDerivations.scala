package org.lasersonlab.zarr.dtype

import org.lasersonlab.zarr.dtype.DataType._
import shapeless._
import shapeless.labelled.FieldType

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
    Name <: Symbol,
    Head,
    Tail <: HList
  ](
    implicit
    head: Aux[Head],
    name: Witness.Aux[Name],
    tail: StructList[Tail]
  ):
    StructList[FieldType[Name, Head] :: Tail]
  = {
    implicit val headEntry =
      StructEntry(
        name.value.name,  // TODO: this is wrong; drops field-name
        head
      )

    StructList(
      headEntry :: tail.entries,
      head.size  + tail.size
    )(
      buff ⇒
        //FieldType
        labelled.field[Name](head(buff)) ::
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
    g: LabelledGeneric.Aux[S, L],
    l: StructList[L]
  ):
    Aux[S] =
    Struct[S, L](l)
}
