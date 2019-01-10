package org.lasersonlab.uri.gcp

import cats._, implicits._, effect._
object TestApp extends IOApp {
  type F[T] = IO[T]
  override def run(args: List[String]): IO[ExitCode] = {
    val expensive: F[Int] = IO.delay{ print("yay") }.map(_ ⇒ 111)

    val delayed: F[F[Int]] = IO.delay { expensive }
    val compute: F[F[Int]] = Concurrent.memoize { expensive }

    val c2: F[Int] = compute.flatMap { _.map { _ * 2 }}
    val c3: F[Int] = compute.flatMap { _.map { _ * 3 }}

    val d2: F[Int] = delayed.flatMap { _.map { _ * 2 }}
    val d3: F[Int] = delayed.flatMap { _.map { _ * 3 }}

    for {
      _ ← compute.flatMap {
        c ⇒
          val c2 = c.map(_ * 2)
          val c3 = c.map(_ * 3)
          for {
            c2 ← c2
            c3 ← c3
          } yield
            println(s"$c2$c3")
      }

      _ ← delayed.flatMap {
        d ⇒
          val d2 = d.map(_ * 2)
          val d3 = d.map(_ * 3)
          for {
            d2 ← d2
            d3 ← d3
          } yield
            println(s"$d2$d3")
      }
    } yield
      ExitCode.Success
  }
}
