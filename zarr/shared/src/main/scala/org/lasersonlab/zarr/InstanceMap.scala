package org.lasersonlab.zarr

import org.hammerlab.shapeless.instances.Instances
import shapeless.HList
import shapeless.ops.hlist.ToTraversable

import scala.reflect.ClassTag

trait InstanceMap[T] {
  def apply(): Map[String, Either[String, T]]
}
object InstanceMap {

  def apply[T]()(implicit i: InstanceMap[T]) = i()

  implicit def instances[T, L <: HList](
    implicit
    i: Instances.Aux[T, L],
    ct: ClassTag[T],
    t: ToTraversable.Aux[L, List, T]
  ):
    InstanceMap[T] =
    new InstanceMap[T] {
      def apply(): Map[String, Either[String, T]] =
        i()
          .toList
          .map {
            o ⇒
              o.toString →
                Right(o)
          }
          .toMap
          .withDefault {
            k ⇒
              Left(
                s"Unrecognized ${ct.runtimeClass.getName}: $k"
              )
          }
    }
}

abstract class HasInstanceMap[T]()(
  implicit
  i: InstanceMap[T]
) {
  val byString: Map[String, Either[String, T]] = i()
}
