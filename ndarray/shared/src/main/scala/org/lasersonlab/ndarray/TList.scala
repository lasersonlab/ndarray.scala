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

trait Map[InList <: TList, Out] {
  type OutList <: TList
  type In
  def apply(in: InList, fn: In ⇒ Out): OutList
}

trait LowPriMap {
  type Aux[InList <: TList, _In, Out, _OutList <: TList] =
    Map[InList, Out] {
      type In = _In
      type OutList = _OutList
    }

  implicit def tnilFlex[InList <: TNil, _In, Out]: Aux[InList, _In, Out, TNil] =
    new Map[InList, Out] {
      type In = _In
      type OutList = TNil
      override def apply(in: InList, fn: In ⇒ Out): TNil = TNil
    }
}
object Map
  extends LowPriMap {

  type Ax[InList <: TList, _In, Out] =
    Map[InList, Out] {
      type In = _In
    }

  implicit def tnilSpecialCase[InList <: TNil, Out]: Aux[InList, Out, Out, TNil] =
    new Map[InList, Out] {
      type In = Out
      type OutList = TNil
      override def apply(in: InList, fn: In ⇒ Out): TNil = TNil
    }

  implicit def cons[
     InList <: TList,
    _In,
     Out,
    _OutList <: TList
  ](
    implicit
    ev: Aux[InList, _In, Out, _OutList]
  ):
    Aux[
      _In :: InList,
      _In,
      Out,
      Out :: _OutList
    ] =
    new Map[_In :: InList, Out] {
      type In = _In
      type OutList = Out :: _OutList
      def apply(in: _In :: InList, fn: In ⇒ Out): OutList =
        in match {
          case h :: t ⇒
            ::(fn(h), ev(t, fn))
        }
    }

  trait Fn[In] {
    type Out
    def apply(in: In): Out
  }
  object Fn {
    type Aux[In, _O] = Fn[In] { type Out = _O }
    implicit def make[In, _Out](fn: In ⇒ _Out): Aux[In, _Out] =
      new Fn[In] {
        type Out = _Out
        def apply(in: In): _Out = fn(in)
      }
  }

  implicit class Ops[InList <: TList](in: InList) {
    def map[In, Out](fn: In ⇒ Out)(implicit ev: Ax[InList, In, Out]): ev.OutList = ev(in, fn)
  }
}
