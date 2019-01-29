package org.lasersonlab.ndview.view

import cats.implicits._
import hammerlab.bytes._
import hammerlab.option._
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
import org.lasersonlab.ndview.model.Login
import org.lasersonlab.ndview.{ ClosedFolders, UpdateDir }
import Entry.`💾`

object Items {
  case class Props(
    login: Login,
    project: Project,
    contents: Contents,
    closedFolders: ?[ClosedFolders]
  )(
    implicit
    val proxy: Proxy,
    val router: Router,
    val config: gcp.Config
  )

  def apply(
    login: Login,
    project: Project,
    contents: Contents,
    closedFolders: ?[ClosedFolders]
  )(
    implicit
    proxy: Proxy,
    router: Router,
    config: gcp.Config
  ): Unmounted[Props, Unit, Unit] =
    component.withKey("items")(
      Props(
        login,
        project,
        contents,
        closedFolders
      )
    )

  val component =
    ScalaComponent
      .builder[Props]("Contents")
      .render_P {
        props ⇒
          import props._
          implicit val Props(login, project, contents, closedFolders) = props
          div(
            cls("items"),
            div(
              cls("dirs"),
            )(
              contents
                .dirs
                .map {
                  dir ⇒
                    div(
                      cls(dir.basename, "dir"),
                      onClick -->
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
                          },
                      Entry(
                        login,
                        project,
                        dir,
                        closedFolders
                          .flatMap {
                            _ / dir.basename
                          }
                      )
                    )
                }
                : _*
            ),
            div(
              cls("objs")
            )(
              contents
                .objs
                .map {
                  case obj @ Obj(_, path, Metadata(_, name, size, _)) ⇒
                    Entry(
                      login,
                      project,
                      obj,
                      None
                    ): VdomNode
                }
                : _*
            )
          )
      }
      .build
}
