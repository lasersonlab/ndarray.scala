package org.lasersonlab.uri

import java.util.MissingResourceException

import cats.implicits._

import utest._

object LocalTest
  extends TestSuite {

  import scala.concurrent.ExecutionContext.Implicits.global

  val tests = Tests{
    'local - {
      resource("test.txt")
        .flatMap(_.string)
        .map {
          actual ⇒
          assert(
            actual ==
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
  }

  def resource(name: String) =
    build
      .resourceDirectories
      .toList
      .map {
        dir ⇒
          Local(s"$dir/$name")
      }
      .map(f ⇒ f.exists.map(f → _))
      .sequence
      .map {
        _
          .find { _._2 }
          . map { _._1 }
          .getOrElse {
            throw new MissingResourceException("", getClass.getName, name)
          }
      }
}
