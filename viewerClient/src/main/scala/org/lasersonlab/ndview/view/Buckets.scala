package org.lasersonlab.ndview.view

import cats.implicits._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import org.lasersonlab.gcp
import org.lasersonlab.gcp.Config.implicits._
import org.lasersonlab.gcp.SignIn
import org.lasersonlab.gcp.googleapis.Paged
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage.Bucket
import org.lasersonlab.ndview.UpdateBucket
import org.lasersonlab.ndview.model.Login

object Buckets
extends SignIn.syntax
{
  case class Props(
    login: Login,
    project: Project,
    buckets: Paged[Bucket]
  )(
    implicit
    val model: ModelProxy[_],
    val config: gcp.Config
  )

  def apply(
    login: Login,
    project: Project,
    buckets: Paged[Bucket]
  )(
    implicit
    model: ModelProxy[_],
    config: gcp.Config
  ) =
    component(
      Props(
        login,
        project,
        buckets
      )
    )

  val component =
    ScalaComponent
      .builder[Props]("Buckets")
      .render_P {
        props ⇒
          import props._
          div(
            key := "buckets",  // TODO: remove key+className duplication boilerplate
            className := "buckets",
            h2("Buckets"),
          )(
            buckets
              .map {
                case bucket @ Bucket(id, name, _, _, contents) ⇒
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
                    contents
                      .map {
                        Contents(login, project, _)

//                        contents ⇒
//                          (
//                            contents
//                              .dirs
//                              .map {
//                                dir ⇒
//                                  div(
//                                    key := dir.name,
//                                    className := "dir entry",
//                                    onClick --> {
//                                      dir
//                                        .ls()
//                                        .fold { Callback() } {
//                                          ΔF ⇒
//                                            Callback.future {
//                                              ΔF
//                                                .map {
//                                                  Δ ⇒
//                                                    model.dispatchCB(
//                                                      UpdateDir(login.id, project.id, dir, Δ)
//                                                    )
//                                                }
//                                                .reauthenticate_?
//                                            }
//                                        }
//                                    }
//                                  )(
//                                    dir.name
//                                  )/*(
//                                    dir
//                                      .contents
//                                      .map {
//                                        contents ⇒
//                                          import contents._
//
//                                      }
//                                  )*/
//                              } ++
//                            contents
//                              .objs
//                              .map {
//                                case Obj(_, path, Metadata(_, name, size, _)) ⇒
//                                  div(
//                                    key := name,
//                                    className := "file entry"
//                                  )(
//                                    s"$name (${Bytes.format(size)})"
//                                  )
//                              }
//                          )
//                          .toVdomArray
                      }
                  )
              }: _*
          )
      }
      .build
}
