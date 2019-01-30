package org.lasersonlab.ndview.view

import cats.implicits._
import io.circe.Printer
import io.circe.generic.auto._
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomArray
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import lasersonlab.diode._
import org.lasersonlab.circe.SingletonCodec._
import org.lasersonlab.gcp.SignIn
import org.lasersonlab.gcp.googleapis.User
import org.lasersonlab.ndview.Main._
import org.lasersonlab.ndview.model.Login
import org.lasersonlab.ndview.{ Model, SelectProject, SelectUserProject, UpdateProjects }

import scala.concurrent.ExecutionContext

// Need this to take precedence over Encoder.encodeIterable; TODO: debug why circe derivation requires this
import org.lasersonlab.gcp.googleapis.Paged.pagedEncoder

object Page
extends SignIn.syntax
{
  case class Props(
    model: Model
  )(
    implicit
    val proxy: Proxy,
    val router: Router,
    val ec: ExecutionContext
  )

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
    import props._
    implicit val Props(Model(logins, _, _)) = props
    logins
      .login
      .fold { Callback() } {
        implicit login: Login ⇒ fetchBuckets
      }
  }

  val component =
    ScalaComponent
      .builder[Props]("Page")
      .render_P {
        props ⇒ import props._
          implicit val Model(logins, route, closedFolders) = model

          val login = logins.login

          println(s"render (${logins.size} logins; project ${login.flatMap(_.project).map(_.name)})")

          div(
            cls("page"),
            div(
              cls("controls"),
              div(
                cls("thumbnails")
              )(
                logins
                  .map {
                    login ⇒
                      val User(id, _, email, picture) = login.user
                      div(
                        cls(id, "thumbnail"),
                        img(
                          cls("avatar"),
                          src := picture
                        ),
                        email
                          .map {
                            email ⇒
                              val domain = email.split("@").last
                              img(
                                cls("domain"),
                                src := s"http://www.google.com/s2/favicons?domain=$domain"
                              )
                          }
                      )
                  }
                  : _*
              )(
                div(
                  cls("add-login"),
                  span(cls("plus"), "+"),
                  onClick --> { SignIn (); Callback() }
                )
              ),
              login
                .map {
                  implicit login ⇒
                    implicit val auth = login.auth

                    VdomArray(
                      div(cls("clear"), onClick --> { LocalStorage.clear(); Callback() }),
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
                    route match {
                      case Vector() ⇒
                        div(
                          cls("tree"),
                          h2("Buckets"),
                          Buckets(
                            login,
                            project,
                            buckets,
                            closedFolders
                          )
                        )
                        : VdomNode
                      case bucket +: path ⇒
                        (
                          for {
                            bucket ← buckets.find(_.name == bucket)
                            entry ← bucket / path
                            contents ← entry.contents
                          } yield
                            div(
                              cls("contents tree"),
                              h2("Contents"),
                              Items(
                                login,
                                project,
                                contents,
                                closedFolders / entry.fullPath
                              )
                            )
                        ): VdomNode
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
        $ ⇒ import $._
          CallbackTo(
            nextProps != currentProps ||
            nextState != currentState
          )
      }
      .componentWillUpdate {
        $ ⇒ import $._
          println("Persisting state to localstorage")
          LocalStorage.save(nextProps.model)
          Callback()
      }
      .componentDidMount  { $ ⇒ checkBuckets($.       props) }
      .componentDidUpdate { $ ⇒ checkBuckets($.currentProps) }
      .build

  def apply(model: Model)(implicit proxy: Proxy, router: Router, ec: ExecutionContext) =
    component.withKey("page")(Props(model))
}
