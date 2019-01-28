package org.lasersonlab.ndview

import java.lang.System.err

import cats.effect.{ ExitCode, IO, IOApp }
import io.circe.generic.auto._
import japgolly.scalajs.react.vdom.Implicits._
import org.lasersonlab.ndview.view.{ LocalStorage, Page }
import org.lasersonlab.gcp.SignIn
import org.scalajs.dom.document

import scala.concurrent.ExecutionContext.Implicits.global

object Main
  extends IOApp
     with SignIn.syntax
{
  override def run(args: List[String]): IO[ExitCode] = {
    println("client main")

    val model = LocalStorage(Model())

    val connection = Circuit(model).connect(_.logins)

    connection(
      implicit model ⇒ Page(model.value)
    )
    .renderIntoDOM(
      document.getElementById("root")
    )

    IO.pure(ExitCode.Success)
  }
}
