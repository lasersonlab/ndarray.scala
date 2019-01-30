package org.lasersonlab.ndview.view

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^._
import org.lasersonlab.ndview.Route

object Path {
  def apply(route: Route)(implicit router: Router) =
    component.withKey("path")(Props(route))

  case class Props(route: Route)(implicit val router: Router)
  val component =
    ScalaComponent
      .builder[Props]("Path")
      .render_P {
        p ⇒ import p._
          div(
            cls("path"),
            span(cls("label"), "Current path:")
          )(
            span(
              router.link(Vector())(
                cls("gs-base", "segment"),
                "gs://"
              )
            ) +:
              route
                .foldLeft(
                  (
                    0,
                    Vector[String](),
                    Vector[TagMod]()
                  )
                ) {
                  case ((idx, prefix, tags), basename) ⇒
                    val path = prefix :+ basename
                    val divider =
                      span(
                        cls(s"$idx-divider", "divider"),
                        "/"
                      )
                    val segment =
                      span(
                        router.link(path)(
                          cls(s"$idx-$basename", "segment"),
                          basename
                        )
                      )
                    (
                      idx + 1,
                      path,
                      if (tags.isEmpty)
                        Vector(segment)
                      else
                        tags ++ Vector(divider, segment)
                    )
                }
                ._3
                : _*
          )
      }
      .build
}
