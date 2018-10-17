package org.lasersonlab.slist

trait Size[A[_]] {
  def apply(a: A[_]): Int
}
object Size {
  implicit def   seq[S[U] <: Seq[U]]: Size[    S] = new Size[    S] { override def apply(a:     S[_]): Int = a.  size }
  implicit def slist[S[U] <:  SList]: Size[    S] = new Size[    S] { override def apply(a:     S[_]): Int = a.  size }
  implicit val   arr                : Size[Array] = new Size[Array] { override def apply(a: Array[_]): Int = a.length }

  implicit class Ops[S[_]](val s: S[_]) extends AnyVal {
    def size(implicit size: Size[S]): Int = size(s)
  }
  trait syntax {
    @inline implicit def slistSizeOps[S[_]](s: S[_]): Ops[S] = Ops(s)
  }
  object syntax extends syntax

  def apply[S[_]](s: S[_])(implicit size: Size[S]): Int = size(s)
}
