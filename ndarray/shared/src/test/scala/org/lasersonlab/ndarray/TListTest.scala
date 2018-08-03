package org.lasersonlab.ndarray

class TListTest
  extends hammerlab.Suite {
  test("basic values / types") {

    // ints:
                   TNil :                      TNil
              1 :: TNil :               Int :: TNil
         2 :: 1 :: TNil :        Int :: Int :: TNil
    3 :: 2 :: 1 :: TNil : Int :: Int :: Int :: TNil

    // strings:
                         TNil :                               TNil
                  "a" :: TNil :                     String :: TNil
           "b" :: "a" :: TNil :           String :: String :: TNil
    "c" :: "b" :: "a" :: TNil : String :: String :: String :: TNil
  }

  import shapeless.test.illTyped

  test("type errors") {
    illTyped("'a :: 1 :: TNil")
    illTyped("'a :: 2 :: 1 :: TNil")
    illTyped("2 :: 'a :: 1 :: TNil")
    illTyped("2 :: 3L :: 1 :: TNil")
  }

  val !! = shapeless.the

  test("zips") {
    val l = 1 :: 2 :: 3 :: TNil
    val r = 2 :: 4 :: 6 :: TNil

    !![Zip[                     TNil,                      TNil]]
    !![Zip[              Int :: TNil,               Int :: TNil]]
    !![Zip[       Int :: Int :: TNil,        Int :: Int :: TNil]]
    !![Zip[Int :: Int :: Int :: TNil, Int :: Int :: Int :: TNil]]

    val zip0 = !![Zip.Aux[                     TNil,                      TNil,                                           TNil]]
    val zip1 = !![Zip.Aux[              Int :: TNil,               Int :: TNil,                             (Int, Int) :: TNil]]
    val zip2 = !![Zip.Aux[       Int :: Int :: TNil,        Int :: Int :: TNil,               (Int, Int) :: (Int, Int) :: TNil]]
    val zip3 = !![Zip.Aux[Int :: Int :: Int :: TNil, Int :: Int :: Int :: TNil, (Int, Int) :: (Int, Int) :: (Int, Int) :: TNil]]

    import Zip.Ops

    zip0(TNil, TNil) should be(TNil)

    zip1(1           :: TNil, 10             :: TNil) should be((1, 10)                       :: TNil)
    zip2(1 :: 2      :: TNil, 10 :: 20       :: TNil) should be((1, 10) :: (2, 20)            :: TNil)
    zip3(1 :: 2 :: 3 :: TNil, 10 :: 20 :: 30 :: TNil) should be((1, 10) :: (2, 20) :: (3, 30) :: TNil)

    1           :: TNil zip('a             :: TNil) should be((1, 'a)                       :: TNil)
    1 :: 2      :: TNil zip('a :: 'b       :: TNil) should be((1, 'a) :: (2, 'b)            :: TNil)
    1 :: 2 :: 3 :: TNil zip('a :: 'b :: 'c :: TNil) should be((1, 'a) :: (2, 'b) :: (3, 'c) :: TNil)
  }

  test("maps") {
    (1 to 10).map(_ * 2)

    !![Map.Aux[TNil, Int, Int, TNil]]

    val l = 1 :: 2 :: 3 :: TNil
    !![Map[TNil, Int]].apply(TNil, _ * 2) should be(TNil)
    !![Map[TNil, String]].apply(TNil, _.toString) should be(TNil)

    !![Map[Int :: Int :: Int :: TNil, Int]].apply(l, _ * 2) should be(2 :: 4 :: 6 :: TNil)
    !![Map[Int :: Int :: Int :: TNil, String]].apply(l, _.toString) should be("1" :: "2" :: "3" :: TNil)

    !![Map.Ax[TNil, Int, Int]]
    !![Map.Aux[TNil, Int, Int, TNil]]

    TNil.map((_: Int) * 2) should be(TNil)
    TNil.map(_.toString) should be(TNil)

    l.map(_ * 2) should be(2 :: 4 :: 6 :: TNil)
    l.map(_.toString) should be("1" :: "2" :: "3" :: TNil)
  }
}
