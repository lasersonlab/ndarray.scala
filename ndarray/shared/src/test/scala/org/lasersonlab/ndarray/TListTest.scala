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
}
