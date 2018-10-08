package org.lasersonlab.ndarray

trait Arithmetic[L, R] {
  def   + ( l: L, r: R ): L
  def   - ( l: L, r: R ): L
  def   * ( l: L, r: R ): L
  def   / ( l: L, r: R ): L
  def   % ( l: L, r: R ): L
  def min ( l: L, r: R ): L
}

object Arithmetic {
  type Id[T] = Arithmetic[T, T]
  implicit val intint: Id[Int] =
    new Arithmetic[Int, Int] {
      def   + (l: Int, r: Int): Int = l + r
      def   - (l: Int, r: Int): Int = l - r
      def   * (l: Int, r: Int): Int = l * r
      def   / (l: Int, r: Int): Int = l / r
      def   % (l: Int, r: Int): Int = l % r
      def min (l: Int, r: Int): Int = Math.min(l, r)
    }

  implicit val longlong: Id[Long] =
    new Arithmetic[Long, Long] {
      def   + (l: Long, r: Long): Long = l + r
      def   - (l: Long, r: Long): Long = l - r
      def   * (l: Long, r: Long): Long = l * r
      def   / (l: Long, r: Long): Long = l / r
      def   % (l: Long, r: Long): Long = l % r
      def min (l: Long, r: Long): Long = Math.min(l, r)
    }

  implicit val longint: Arithmetic[Long, Int] =
    new Arithmetic[Long, Int] {
      def   + (l: Long, r: Int): Long = l + r
      def   - (l: Long, r: Int): Long = l - r
      def   * (l: Long, r: Int): Long = l * r
      def   / (l: Long, r: Int): Long = l / r
      def   % (l: Long, r: Int): Long = l % r
      def min (l: Long, r: Int): Long = Math.min(l, r)
    }

  implicit class Ops[L](val l: L) extends AnyVal {
    def   + [R] (r: R)(implicit a: Arithmetic[L, R]): L = a.  +(l, r)
    def   - [R] (r: R)(implicit a: Arithmetic[L, R]): L = a.  -(l, r)
    def   * [R] (r: R)(implicit a: Arithmetic[L, R]): L = a.  *(l, r)
    def   / [R] (r: R)(implicit a: Arithmetic[L, R]): L = a.  /(l, r)
    def   % [R] (r: R)(implicit a: Arithmetic[L, R]): L = a.  %(l, r)
    def min [R] (r: R)(implicit a: Arithmetic[L, R]): L = a.min(l, r)
  }

  trait syntax {
    @inline implicit def arithmeticOps[L](l: L): Ops[L] = Ops(l)
  }
  object syntax extends syntax
}
