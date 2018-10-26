package org.lasersonlab

import java.nio.ByteBuffer

import cats.effect.IO
import org.lasersonlab.uri.Local

import scala.scalajs.js.typedarray.{ ArrayBuffer, Int8Array, Uint8Array }
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.scalajs.js

class Test
  extends hammerlab.Suite {
  test("fs") {
    ==(
      new String(Local[IO](".gitignore").read.unsafeRunSync()),
      """*.log
        |10x_parquet
        |dependency-reduced-pom.xml
        |files
        |metastore_db
        |target"""
        .stripMargin
    )
//    import scalajs.js.Dynamic.{ global ⇒ g }
//    val fs = g.require("fs")
//    val f = ".gitignore"
//    val stat = fs.statSync(f)
//    val size = stat.size.asInstanceOf[Int]
//    val fd = fs.openSync(f, "r")
//    val arr = new Int8Array(size)
//    fs.readSync(fd, arr, 0, size, 0)
//
//    println(new String(arr.toArray))
    //buffer.asCharBuffer()
  }
}
