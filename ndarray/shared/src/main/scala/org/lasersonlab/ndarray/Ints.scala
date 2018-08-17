package org.lasersonlab.ndarray

import hammerlab.shapeless.tlist._

object Ints {
  type Ints1 = Int ::  TNil
  type Ints2 = Int :: Ints1
  type Ints3 = Int :: Ints2
  type Ints4 = Int :: Ints3
  type Ints5 = Int :: Ints4
  type Ints6 = Int :: Ints5
}
