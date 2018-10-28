package org.lasersonlab.ndviewer.server

import cats.data.{ NonEmptyList, OptionT }
import cats.effect._
import cats.implicits._
import fs2._
import org.http4s.CacheDirective.`no-cache`
import org.http4s.{ Header, HttpRoutes, MediaType }
import org.http4s.server.blaze.BlazeServerBuilder
//import org.http4s.MediaType.`text/html`
import org.http4s.dsl.io._
import org.http4s.headers.{ `Cache-Control`, `Content-Type` }
import org.http4s.server.blaze._
import org.http4s.{ Charset, HttpService, StaticFile }
import scalatags.Text.TypedTag
import scalatags.Text.all.Modifier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Properties.envOrNone

object Main extends IOApp {

  val ip: String = "0.0.0.0"

//  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] =
//    for {
//      port <- Stream.eval(IO(envOrNone("HTTP_PORT").map(_.toInt).getOrElse(8080)))
//      exitCode <- BlazeBuilder[IO]
//        .bindHttp(port, ip)
//        .mountService(service)
//        .serve
//    } yield exitCode

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(service)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  def template(
    headContent: Seq[Modifier] = Nil,
    bodyContent: Seq[Modifier] = Nil,
        scripts: Seq[Modifier] = Nil,
       cssComps: Seq[Modifier] = Nil
  ):
    TypedTag[String] =
  {
    import scalatags.Text.all._

    html(
      head(
        headContent,
        cssComps
      ),
      body(
        bodyContent,
        scripts
      )
    )
  }

  val jsScript = "viewerclient-fastopt-bundle.js"
  val jsScripts: Seq[Modifier] = {
    import scalatags.Text.all._
    List(
      script(src := jsScript),
    )
  }

  val index: Seq[Modifier] = {
    import scalatags.Text.all._
    Seq(
      div(
        id:="root"
      ),
      div(
        id:="button"
      ),
      h1(
        style:= "align: center;",
        "Http4s Scala-js Example App"
      ),
      a(href:="/button", h4("Button Example")),
      a(href:="/ajax", h4("Ajax Example"))
    )
  }

  val supportedStaticExtensions =
    List(".html", ".js", ".map", ".css", ".png", ".ico")

  val text = "ab⊥"

  def service = {
    def getResource(pathInfo: String) = IO.delay(getClass.getResource(pathInfo))

    HttpRoutes.of[IO] {

      case GET -> Root =>
        Ok(template(Seq(), index, jsScripts).render)
          .map(
            _.withContentType(`Content-Type`(MediaType.text.html, Charset.`UTF-8`))
              .putHeaders(`Cache-Control`(NonEmptyList.of(`no-cache`())))
          )

      case req @ GET -> Root / "uri" =>
        println(
          s"*** Entries:\n\t${
            req
              .attributes
              .entries
              .mkString("\n\t")
          }"
        )
        println(s"*** Headers:\n\t${req.headers.toList.mkString("\n\t")}")
        Ok(template(Seq(), index, jsScripts).render)
          .map(
            _.withContentType(`Content-Type`(MediaType.text.html, Charset.`UTF-8`))
             .putHeaders(`Cache-Control`(NonEmptyList.of(`no-cache`())))
          )

      case req @ HEAD -> Root / "ajax" ⇒
        println("ajax…")
        NoContent().putHeaders(Header("Content-Length", (text.length + 1).toString))
        //Ok("yay")

      case req @ GET -> Root / "ajax" ⇒
        println("ajax…")
        Ok.apply(text)

      case req if supportedStaticExtensions.exists(req.pathInfo.endsWith) =>
        println(req.pathInfo)
        StaticFile.fromResource[IO](req.pathInfo, global, req.some)
          .orElse(OptionT.liftF(getResource(req.pathInfo)).flatMap(StaticFile.fromURL(_, global, req.some)))
          .map(_.putHeaders(`Cache-Control`(NonEmptyList.of(`no-cache`()))))
          .fold(NotFound())(_.pure[IO])
          .flatten

    }
    .orNotFound
  }
}
