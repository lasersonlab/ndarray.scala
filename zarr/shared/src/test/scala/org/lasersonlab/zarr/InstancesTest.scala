package org.lasersonlab.zarr

import org.lasersonlab.zarr.ByteOrder.{ BigEndian, LittleEndian, None }
import shapeless._

case class Foo[T](bo: ByteOrder, dt: DType)

class InstancesTest
  extends hammerlab.Suite {
  test("instances") {
    import DType._
    implicitly[Instances[DType]].apply() should be(bool :: float :: int :: string :: unicode :: HNil)
    implicitly[Instances[ByteOrder]].apply() should be(BigEndian :: LittleEndian :: None :: HNil)

    implicitly[
      Cartesian[
        Int :: HNil,
        String :: HNil
      ]
    ]
    .apply(
      1 :: HNil,
      "a" :: HNil
    ) should be(
      (1 :: "a" :: HNil) ::
      HNil
    )

    implicitly[
      Cartesian[
        Int :: Boolean :: HNil,
        String :: HNil
      ]
    ]
    .apply(
      1 :: true :: HNil,
      "a" :: HNil
    ) should be(
      (1 :: "a" :: HNil) ::
      (true :: "a" :: HNil) ::
      HNil
    )

    implicitly[
      Cartesian[
        Int :: Boolean :: HNil,
        String :: Long ::  HNil
      ]
    ]
    .apply(
      1 :: true :: HNil,
      "a" :: 3L :: HNil
    ) should be(
      (1 :: "a" :: HNil) ::
      (1 :: 3L :: HNil) ::
      (true :: "a" :: HNil) ::
      (true :: 3L :: HNil) ::
      HNil
    )

    val > = BigEndian
    val < = LittleEndian
    val | = None

    val b = bool
    val f = float
    val i = int
    val S = string
    val U = unicode

    implicitly[Instances[Foo[_]]].apply() should be(
      (> :: b :: HNil) ::
      (> :: f :: HNil) ::
      (> :: i :: HNil) ::
      (> :: S :: HNil) ::
      (> :: U :: HNil) ::
      (< :: b :: HNil) ::
      (< :: f :: HNil) ::
      (< :: i :: HNil) ::
      (< :: S :: HNil) ::
      (< :: U :: HNil) ::
      (| :: b :: HNil) ::
      (| :: f :: HNil) ::
      (| :: i :: HNil) ::
      (| :: S :: HNil) ::
      (| :: U :: HNil) ::
      HNil
    )
  }
}
