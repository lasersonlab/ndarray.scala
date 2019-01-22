package org.lasersonlab.ndview

import cats.effect.{ ExitCode, IO, IOApp }
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
    Page().renderIntoDOM(document.getElementById("root"))
    IO.pure(ExitCode.Success)
  }
}
