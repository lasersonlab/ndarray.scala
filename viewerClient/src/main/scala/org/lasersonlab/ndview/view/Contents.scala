package org.lasersonlab.ndview.view

import cats.implicits._
import hammerlab.bytes.Bytes
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import lasersonlab.diode._
import org.lasersonlab.gcp
import org.lasersonlab.gcp.Config.implicits._
import org.lasersonlab.gcp.Metadata
import org.lasersonlab.gcp.SignIn.syntax._
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage.{ Contents, Obj }
import org.lasersonlab.ndview.UpdateDir
import org.lasersonlab.ndview.model.Login
import org.lasersonlab.ndview.view.Page.Router

object Contents {
  case class Props(
    login: Login,
    project: Project,
    contents: Contents
  )(
    implicit
    val proxy: Proxy,
    val router: Router,
    val config: gcp.Config
  )

  def apply(
    login: Login,
    project: Project,
    contents: Contents
  )(
    implicit
    proxy: Proxy,
    router: Router,
    config: gcp.Config
  ): Unmounted[Props, Unit, Unit] =
    component.withKey("contents")(
      Props(
        login,
        project,
        contents
      )
    )

  val component =
    ScalaComponent
      .builder[Props]("Contents")
      .render_P {
        props ⇒
          import props._
          implicit val Props(login, project, contents) = props
          div(
            key := "contents",
            className := "contents"
          )(
            contents
              .dirs
              .map {
                dir ⇒
                  div(
                    key := dir.basename,
                    className := "dir entry",
                    onClick --> {
                      dir
                        .ls()
                        .fold { Callback() } {
                          ΔF ⇒
                            Callback.future {
                              ΔF
                                .map {
                                  Δ ⇒
                                    UpdateDir(login.id, project.id, dir, Δ).dispatch
                                }
                                .reauthenticate_?
                            }
                        }
                    }
                  )(
                    router.link(dir.fullPath)(dir.basename)
                  )(
                    dir
                      .contents
                      .map {
                        contents ⇒

                          Contents(
                            login, project, contents
                          )
                      }
                  )
              } ++
            contents
              .objs
              .map {
                case obj @ Obj(_, path, Metadata(_, name, size, _)) ⇒
                  div(
                    key := obj.basename,
                    className := "file entry"
                  )(
                    s"${obj.basename} (${Bytes.format(size)})"
                  )
              }
            : _*
          )
      }
      .build
}
