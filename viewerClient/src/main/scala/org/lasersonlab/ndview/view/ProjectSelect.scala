package org.lasersonlab.ndview.view

import cats.implicits._
import io.circe.generic.auto._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.ndview.model.Login
import org.lasersonlab.uri.?
import org.scalajs.dom.raw.HTMLSelectElement

// Need this to take precedence over Encoder.encodeIterable
import org.lasersonlab.gcp.googleapis.Paged.pagedEncoder

object ProjectSelect {
  case class Props(
    login: Login,
    update: String ⇒ Callback,
    project: Option[Project],
    placeholder: String,
  )
  val component =
    ScalaComponent
      .builder[Props]("ProjectSelect")
      .render_P {
        case Props(login, update, project, placeholder) ⇒
          val projects = login.projects
          select(
            key := placeholder,
            value := project.fold[String] { "" } { _.id },
            onChange ==> {
              e ⇒
                update(
                  e
                    .target
                    .asInstanceOf[HTMLSelectElement]
                    .value
                )
            }
          )(
            (
              option(
                key := "_default",
                value := "",
                disabled := true,
                  hidden := true
              )(
                s"$placeholder"
              ) +:
              projects
                .map {
                  case Project(name, id, _, _) ⇒
                    option(
                        key := id,
                      value := id
                    )(
                      name
                    )
                }
            ) toVdomArray
          )
      }
      .build

  def apply(
    login: Login,
    update: String ⇒ Callback,
    project: Option[Project],
    placeholder: String,
    key: ?[String] = None
  ) =
    component
      .withKey(
        key.getOrElse(placeholder)
      )
      .apply(
        Props(
          login,
          update,
          project,
          placeholder
        )
      )
}
