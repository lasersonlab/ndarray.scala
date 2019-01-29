package org.lasersonlab.ndview.view

import cats.implicits._
import io.circe.Printer
import io.circe.generic.auto._
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.VdomArray
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import lasersonlab.diode._
import org.lasersonlab.circe.SingletonCodec._
import org.lasersonlab.gcp.SignIn
import org.lasersonlab.ndview.Main._
import org.lasersonlab.ndview.model.Login
import org.lasersonlab.ndview.{ Model, SelectProject, SelectUserProject, UpdateProjects }

import scala.concurrent.ExecutionContext

// Need this to take precedence over Encoder.encodeIterable; TODO: debug why circe derivation requires this
import org.lasersonlab.gcp.googleapis.Paged.pagedEncoder

object Page
extends SignIn.syntax
{
  type Route = Vector[String]
  type Router = RouterCtl[Route]
  type Props = (Model, Proxy, Router, ExecutionContext)

  val pprint = Printer.spaces4.copy(colonLeft = "").pretty _

  def fetchBuckets(
    implicit
    login: Login,
    proxy: Proxy,
    ec: ExecutionContext
  ) = {
    println(s"checking buckets for login ${login.id}")
    implicit val auth = login.auth
    login
      .projects
      .fetchBuckets
      .fold { Callback() } {
        ΔF ⇒
          Callback.future {
            ΔF
              .map {
                Δ ⇒
                  println("got projects post-bucket-fetch")
                  UpdateProjects(login.id, Δ).dispatch
              }
          }
      }
  }

  def checkBuckets(props: Props) = {
    implicit val (Model(logins, _), proxy, _, ec) = props
    logins
      .login
      .fold { Callback() } {
        implicit login: Login ⇒ fetchBuckets
      }
  }

  val component =
    ScalaComponent
      .builder[Props]("Page")
      .render {
        $ ⇒ import $._
          implicit val (model @ Model(logins, route), proxy, router, ec) = props

          val login  = logins.login

          println(s"render (${logins.size} logins; project ${login.flatMap(_.project).map(_.name)})")

          div(
            cls("page"),
            div(
              cls("controls")
            )(
              button(key := "sign-in", onClick --> { SignIn (); Callback() }, "sign in" ),
              login
                .map {
                  implicit login ⇒
                    implicit val auth = login.auth

                    VdomArray(
                      button(key := "sign-out", onClick --> { LocalStorage.clear(); Callback() }, "clear state"),
                      ProjectSelect(login, id ⇒ fetchBuckets *>     SelectProject(id), login.    project,         "Project"),
                      ProjectSelect(login, id ⇒ fetchBuckets *> SelectUserProject(id), login.userProject, "Bill-to Project"),
                    )
                },
            ),

            Path(route),

            login
              .fold { TagMod() } {
                login ⇒
                  implicit val Login(auth, user, projects, userProject) = login
                  for {
                    project ← login.project
                    buckets ← project.buckets
                  } yield {
                    println(s"${buckets.size} buckets, path $route (${route.toList}, ${route.size})")
                    route.toList match {
                      case Nil ⇒
                        println("Nil")
                        div(
                          cls("buckets tree"),
                          h2("Buckets"),
                          Buckets(
                            login,
                            project,
                            buckets
                          )
                        )
                        : VdomNode
                      case bucket :: path ⇒
                        println(s"$bucket :: $path")
                        (
                          for {
                            bucket ← buckets.find(_.name == bucket)
                            entry ← bucket / path
                            contents ← entry.contents
                          } yield
                            div(
                              cls("contents tree"),
                              h2("Contents"),
                              Contents(login, project, contents)
                            )
                        )
                        .toList
                        .toVdomArray
                    }
                  }
              },

            div(
              cls("state")
            )(
              h4("State (Debug)"),
              Json(model.asJson)
            )
          )
      }
      .shouldComponentUpdate {
        p ⇒
          import p._
          CallbackTo(
            nextProps != currentProps ||
            nextState != currentState
          )
      }
      .componentWillUpdate {
        p ⇒
          println("Persisting state to localstorage")
          LocalStorage.save(p.nextProps._1)
          Callback()
      }
      .componentDidMount  { $ ⇒ checkBuckets($.       props) }
      .componentDidUpdate { $ ⇒ checkBuckets($.currentProps) }
      .build

  def apply(model: Model)(implicit proxy: Proxy, router: Router, ec: ExecutionContext) = component((model, proxy, router, ec))
}
