package lasersonlab

import org.lasersonlab.slist.{ Scannable, Size, Zip, syntax, SList ⇒ s }

/**
 * Exported interface for [[org.lasersonlab.slist.SList]]
 */
trait slist
  extends s.instances
     with syntax {

  type SList = s
  val  SList = s

  val `0` = s.`0`
  val  ⊥  = s.`0`
  type ⊥  = `0`.type

  type `0`[T] = s.`0`[T]
  type `1`[T] = s.`1`[T]
  type `2`[T] = s.`2`[T]
  type `3`[T] = s.`3`[T]
  type `4`[T] = s.`4`[T]
  type `5`[T] = s.`5`[T]
  type `6`[T] = s.`6`[T]
  type `7`[T] = s.`7`[T]
  type `8`[T] = s.`8`[T]
  type `9`[T] = s.`9`[T]

  @inline implicit def SListOps[T, Tail[_]](t: Tail[T]): s.Ops[T, Tail] = s.Ops(t)

  type Cons[In[_]] = s.Cons[In]
  val Cons = s.Cons

  val :: = s.::
}
object slist extends slist
