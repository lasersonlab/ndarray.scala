package org.lasersonlab.ndview.view

import cats.implicits._
import diode.react.ModelProxy
import hammerlab.bytes.Bytes
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import org.lasersonlab.gcp
import org.lasersonlab.gcp.Config.implicits._
import org.lasersonlab.gcp.googleapis.Paged
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage.Bucket
import org.lasersonlab.gcp.{ Metadata, SignIn }
import org.lasersonlab.ndview.UpdateBucket
import org.lasersonlab.ndview.model.Login

object Buckets
extends SignIn.syntax
{
  case class Props(
    model: ModelProxy[_],
    login: Login,
    project: Project,
    buckets: Paged[Bucket],
    //dispatch:
  )(
    implicit val config: gcp.Config
  )

  def apply(
    model: ModelProxy[_],
    login: Login,
    project: Project,
    buckets: Paged[Bucket]
  )(
    implicit config: gcp.Config
  ) =
    component(
      Props(
        model,
        login,
        project,
        buckets
      )
    )

  val component =
    ScalaComponent
      .builder[Props]("Buckets")
      .render_P {
        case props @ Props(model, login, project, buckets) ⇒
          import props.config
          implicit val Login(auth, user, projects, userProject) = login
          div(
            key := "buckets",  // TODO: remove key+className duplication boilerplate
            className := "buckets"
          )(
            buckets
              .map {
                case bucket @ Bucket(id, name, _, _, objects) ⇒
                  div(
                    key := id,
                    className := "bucket",
                    onClick --> {
                      println("clicked")
                      bucket
                        .ls()
                        .fold { Callback() } {
                          ΔF ⇒
                            Callback.future {
                              ΔF
                                .map {
                                  Δ ⇒
                                    model.dispatchCB(
                                      UpdateBucket(login.id, project.id, id, Δ)
                                    )
                                }
                                .reauthenticate_?
                            }
                        }
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
