package org.lasersonlab.ndarray

import org.hammerlab.shapeless.tlist._
import shapeless.Lazy

trait ScanRight[InList, OutElem] {
  type InElem
  type OutList
  def apply(
    inlist: InList,
    init: OutElem,
    fn: (InElem, OutElem) ⇒ OutElem
  ):
    (
      OutElem,
      OutList
    )
}
object ScanRight {
  type Aux[InList, _InElem, OutElem, _OutList] =
    ScanRight[InList, OutElem] {
      type InElem = _InElem
      type OutList = _OutList
    }

  def make[
     InList,
    _InElem,
     OutElem,
    _OutList
  ](
    f: (
      InList,
      OutElem,
      (_InElem, OutElem) ⇒ OutElem
    ) ⇒ (
      OutElem,
      _OutList
    )
  ): Aux[InList, _InElem, OutElem, _OutList] =
    new ScanRight[InList, OutElem] {
      type InElem = _InElem
      type OutList = _OutList
      def apply(
        inlist: InList,
        init: OutElem,
        fn: (InElem, OutElem) ⇒ OutElem
      ): (
        OutElem,
        OutList
      ) =
        f(inlist, init, fn)
    }

  implicit def seq[A, B]: Aux[Seq[A], A, B, Seq[B]] =
    make {
      (inlist, init, fn) ⇒
        val scanned = inlist.scanRight(init)(fn)
        (
          scanned.head,
          scanned.tail
        )
    }

  implicit def hnil[InElem, OutElem]: Aux[TNil, InElem, OutElem, TNil] =
    make(
      (inlist, init, fn) ⇒ (init, inlist)
    )

  implicit def cons[
     InList <: TList,
     InElem,
    OutElem,
    OutList <: TList
  ](
    implicit
    s:
      Lazy[
        Aux[
           InList,
           InElem,
          OutElem,
          OutList
        ]
      ],
     inpp: Prepend[InElem, InList],
    outpp: Prepend[OutElem, OutList]
  ):
    Aux[
       InElem :: InList,
       InElem,
      OutElem,
      OutElem :: OutList
    ] =
    make {
      case (h :: t, init, fn) ⇒
        val (prevTotal, prevOut) = s.value(t, init, fn)

        (
          fn(h, prevTotal),
          prevTotal :: prevOut
        )
    }

  //implicit class Ops
}
