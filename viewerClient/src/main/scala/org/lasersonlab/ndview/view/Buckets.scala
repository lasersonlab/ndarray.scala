package org.lasersonlab.ndview.view

import cats.ApplicativeError
import cats.implicits._
import hammerlab.bytes.Bytes
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.{ TagOf, VdomArray }
import org.lasersonlab.ndview.model.Login
import org.lasersonlab.gcp
import org.lasersonlab.gcp.Config.implicits._
import org.lasersonlab.gcp.{ Metadata, SignIn }
import org.lasersonlab.gcp.googleapis.Paged
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage.{ Bucket, Buckets }

import scala.concurrent.{ ExecutionContext, Future }

object Buckets
extends SignIn.syntax
{
  case class Props(
    login: Login,
    project: Project,
    buckets: Paged[Bucket],
    update: (Login ⇒ Login) ⇒ Callback
  )(
    implicit val config: gcp.Config
  )

  def apply(
    login: Login,
    project: Project,
    buckets: Paged[Bucket],
    update: (Login ⇒ Login) ⇒ Callback
  )(
    implicit config: gcp.Config
  ) =
    component(
      Props(
        login,
        project,
        buckets,
        update
      )
    )

  val component =
    ScalaComponent
      .builder[Props]("Buckets")
      .render_P {
        case props @ Props(login, project, buckets, update) ⇒
          import props.config
          implicit val Login(auth, user, projects, userProject) = login
          div(
            key := "buckets",
            className := "buckets"
          )(
            buckets
              .map {
                case bucket @ Bucket(id, name, _, _, objects) ⇒
                  implicitly[ExecutionContext]
                  implicitly[ApplicativeError[Future, Throwable]]
                  div(
                    key := id,
                    className := "bucket",
                    onClick -->
                      Callback.future {
                        println("clicked")
                        bucket
                          .ls()
                          .map {
                            next ⇒
                              println(s"Got new bucket: $next")
                              update(
                                _
                                  .copy(
                                    projects =
                                      projects
                                        .mod(project.id) {
                                            _
                                              .copy(
                                                buckets =
                                                  Some(
                                                    buckets
                                                      .copy(
                                                        items =
                                                          buckets
                                                            .items
                                                            .map {
                                                              case b if b.id == bucket.id ⇒ next
                                                              case b ⇒ b
                                                            }
                                                      )
                                                  )
                                              )
                                        }
                                  )
                              )
                          }
                          .reauthenticate_?
                      }
                  )(
                    name,
                    objects
                      .map {
                        objects ⇒
                          (
                            objects
                              .dirs
                              .map {
                                dir ⇒
                                  div(
                                    key := dir,
                                    className := "dir"
                                  )(
                                    dir
                                  )
                              } ++
                            objects
                              .files
                              .map {
                                case Metadata(_, name, size, _) ⇒
                                  div(
                                    key := name,
                                    className := "file"
                                  )(
                                    s"$name (${Bytes.format(size)})"
                                  )
                              }
                          )
                          .toVdomArray
                      }
                  )
              }: _*
          )
      }
      .build
}
