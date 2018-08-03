package org.lasersonlab.ndarray

import org.lasersonlab.ndarray

sealed trait TList {
  type T
}
object TList {
  type Aux[_T] = TList { type T = _T }
  implicit class Ops[L <: TList](val l: L) extends AnyVal {
    def ::[T](t: T)(implicit ev: Prepend[T, L]) = ev(t, l)
  }
}

sealed trait TNil extends TList {
  type T = Nothing
  def ::[T](t: T) = ndarray.::(t, this)
}
case object TNil extends TNil

case class ::[_T, Tail <: TList](head: _T, tail: Tail) extends TList {
  type T = _T
  def ::[U >: this.type <: TList.Aux[T]](t: T): ::[T, U] = ndarray.::(t, this: U)
}

trait Zip[L <: TList, R <: TList] {
  type Out <: TList
  def apply(l: L, r: R): Out
}
object Zip {

  implicit class Ops[L <: TList](val l: L) extends AnyVal {
    def zip[R <: TList](r: R)(implicit z: Zip[L, R]): z.Out = z(l, r)
  }

  type Aux[L <: TList, R <: TList, _O <: TList] = Zip[L, R] { type Out = _O }
  implicit val tnil: Aux[TNil, TNil, TNil] =
    new Zip[TNil, TNil] {
      type Out = TNil
      override def apply(l: TNil, r: TNil): TNil = l
    }

  implicit def cons[
    EL,
    ER,
    L <: TList,
    R <: TList
  ](
    implicit
    ev: Zip[L, R]
  ):
    Aux[
      EL :: L,
      ER :: R,
      (EL, ER) :: ev.Out
    ] =
    new Zip[EL :: L, ER :: R] {
      type Out = (EL, ER) :: ev.Out
      def apply(
        l: EL :: L,
        r: ER :: R
      ):
        (EL, ER) :: ev.Out =
        ::(
          (l.head, r.head),
          ev(l.tail, r.tail)
        )
    }
}

trait Map[InList <: TList] {
  type OutList <: TList
  type In
  type Out
  def apply(in: InList, fn: In ⇒ Out): OutList
}
object Map {
  type Aux[InList <: TList, _In, _Out, _OutList <: TList] =
    Map[InList] {
      type In = _In
      type Out = _Out
      type OutList = _OutList
    }

  implicit def tnil[_In, _Out]: Aux[TNil, _In, _Out, TNil] =
    new Map[TNil] {
      type In = _In
      type Out = _Out
      type OutList = TNil
      override def apply(in: TNil, fn: _In ⇒ _Out): TNil = TNil
    }

  implicit def cons[
     InList <: TList,
    _In,
    _Out,
    _OutList <: TList
  ](
    implicit
    ev: Aux[InList, _In, _Out, _OutList]
  ):
    Aux[
      _In :: InList,
      _In,
      _Out,
      _Out :: _OutList
    ] =
    new Map[_In :: InList] {
      type In = _In
      type Out = _Out
      type OutList = _Out :: _OutList
      def apply(in: _In :: InList, fn: In ⇒ Out): OutList =
        in match {
          case h :: t ⇒
            ::(fn(h), ev(t, fn))
        }
    }

  implicit class Ops[InList <: TList](val in: InList) extends AnyVal {
    def map[In, Out, OutList <: TList](fn: In ⇒ Out)(implicit ev: Aux[InList, In, Out, OutList]): OutList = ev(in, fn)
  }
}
