package org.lasersonlab.ndview.view

import cats.implicits._
import hammerlab.option._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import lasersonlab.diode._
import org.lasersonlab.gcp
import org.lasersonlab.gcp.Config.implicits._
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage
import org.lasersonlab.gcp.googleapis.storage.Obj
import org.lasersonlab.ndview.{ CloseDir, ClosedFolders, OpenDir }
import org.lasersonlab.ndview.model.Login

object Entry {
  case class Props(
    login: Login,
    project: Project,
    path: storage.Path,
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
    path: storage.Path,
    closedFolders: ?[ClosedFolders]
  )(
    implicit
    proxy: Proxy,
    router: Router,
    config: gcp.Config
  ) =
    component.withKey(path.basename)(
      Props(
        login,
        project,
        path,
        closedFolders
      )
    )

  val `📂` = "\uD83D\uDCC2"
  val `📁` = "\uD83D\uDCC1"
  val `💾` = "\uD83D\uDCBE"

  def icon(path: storage.Path, closed: Boolean)(implicit proxy: Proxy) =
    span(
      cls("icon")
    )(
      (
        path match {
          case _: Obj ⇒ Vector[TagMod](`💾`)
          case _ ⇒
            if (closed) Vector[TagMod](`📁`, onClick -->  OpenDir(path).dispatch)
            else        Vector[TagMod](`📂`, onClick --> CloseDir(path).dispatch)
        }
      )
      : _*
    )

  val component =
    ScalaComponent
      .builder[Props]("Entry")
      .render_P {
        props ⇒
          import props._
          implicit val Props(login, project, path, closedFolders) = props
          val closed = closedFolders.fold { false } { _.value.getOrElse(false) } || path.contents.isEmpty
          val `type` =
            path match {
              case _: Obj ⇒ "file"
              case _ ⇒ "dir"
            }
          div(
            cls(s"${`type`} entry"),
            span(
              cls("name"),
              icon(path, closed),
              router.link(path.fullPath)(path.basename),
            ),
            for {
              contents ← path.contents
              if !closed
            } yield
              Items(login, project, contents, closedFolders)
          )
      }
      .build
}
