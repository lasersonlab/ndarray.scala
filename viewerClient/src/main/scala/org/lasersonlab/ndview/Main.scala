package org.lasersonlab.ndview

import cats.effect.{ ExitCode, IO, IOApp }
import japgolly.scalajs.react.vdom.Implicits._
import org.lasersonlab.ndview.view.Page
import org.lasersonlab.gcp.SignIn
import org.scalajs.dom.document

import scala.concurrent.ExecutionContext.Implicits.global

object Main
  extends IOApp
     with SignIn.syntax
{
  override def run(args: List[String]): IO[ExitCode] = {
    println("client main")
    val connection = Circuit.connect(_.logins)

    connection(
      Page(_)
    )
    .renderIntoDOM(
      document.getElementById("root")
    )

    IO.pure(ExitCode.Success)
  }
}
