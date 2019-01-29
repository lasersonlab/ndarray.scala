package org.lasersonlab.ndview.view

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import lasersonlab.diode._
import org.lasersonlab.gcp
import org.lasersonlab.gcp.Config.implicits._
import org.lasersonlab.gcp.SignIn
import org.lasersonlab.gcp.googleapis.Paged
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage.Bucket
import org.lasersonlab.ndview.UpdateBucket
import org.lasersonlab.ndview.model.Login
import org.lasersonlab.ndview.view.Page.Router

object Buckets
extends SignIn.syntax
{
  case class Props(
    login: Login,
    project: Project,
    buckets: Paged[Bucket]
  )(
    implicit
    val proxy: Proxy,
    val router: Router,
    val config: gcp.Config
  )

  def apply(
    login: Login,
    project: Project,
    buckets: Paged[Bucket]
  )(
    implicit
    proxy: Proxy,
    router: Router,
    config: gcp.Config
  ) =
    component.withKey("buckets")(
      Props(
        login,
        project,
        buckets
      )
    )

  // 📁📂

  val component =
    ScalaComponent
      .builder[Props]("Buckets")
      .render_P {
        props ⇒
          import props._
          div(
            cls("items")
          )(
            buckets
              .map {
                case bucket @ Bucket(id, name, _, _, contents) ⇒
                  div(
                    cls(id, "bucket"),
                    onClick --> {
                      bucket
                        .ls()
                        .fold { Callback() } {
                          ΔF ⇒
                            Callback.future {
                              ΔF
                                .map {
                                  Δ ⇒
                                    UpdateBucket(login.id, project.id, id, Δ).dispatch
                                }
                                .reauthenticate_?
                            }
                        }
                      }
                  )(
                    router.link(bucket.fullPath)(name),
                    contents
                      .map {
                        Contents(login, project, _)
                      }
                  )
              }: _*
          )
      }
      .build
}
