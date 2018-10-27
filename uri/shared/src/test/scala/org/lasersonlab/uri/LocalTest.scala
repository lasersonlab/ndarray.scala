package org.lasersonlab.uri

import java.util.MissingResourceException

import cats.effect.IO
import cats.implicits._

class LocalTest
  extends hammerlab.Suite {

  def resource(name: String) =
    build
      .resourceDirectories
      .map {
        dir ⇒
          Local[IO](s"$dir/$name")
      }
      .find(_.exists.unsafeRunSync)
      .getOrElse {
        throw new MissingResourceException("", getClass.getName, name)
      }

  test("local") {
    ==(
      resource("test.txt").string.unsafeRunSync,
      """a
        |12
        |abc
        |1234
        |abcde
        |"""
        .stripMargin
    )
  }
}
