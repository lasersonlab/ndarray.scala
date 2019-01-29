package org.lasersonlab.ndview.view

import cats.implicits._
import io.circe.JsonNumber
import io.{ circe ⇒ c }
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^.{ VdomNode, _ }
import org.lasersonlab.uri.?

object Json {
  object Null { def unapply(json: c.Json): Option[                    Unit    ] = json.asNull                   }
  object  Num { def unapply(json: c.Json): Option[              JsonNumber    ] = json.asNumber                 }
  object  Str { def unapply(json: c.Json): Option[                  String    ] = json.asString                 }
  object Bool { def unapply(json: c.Json): Option[                 Boolean    ] = json.asBoolean                }
  object  Arr { def unapply(json: c.Json): Option[ Vector[          c.Json  ] ] = json.asArray                  }
  object  Obj { def unapply(json: c.Json): Option[ Vector[ (String, c.Json) ] ] = json.asObject.map(_.toVector) }

  case class Props(
    json: c.Json,
    field: ?[String] = None,
    root: Boolean = true
  )

  val component =
    ScalaComponent
      .builder[Props]("Json")
      .render_P {
        case props @ Props(json, _, root) ⇒
          def label(str: String): VdomNode =
            props
              .field
              .fold {
                str
              } {
                field ⇒ s"$field: $str"
              }

          def  open(str: String): VdomNode = div(key := "open")(label(str))
          def close(str: String): VdomNode = div(key := "close")(str)

          div(
            className := s"json${if (root) "" else " indent"}",
            json match {
              case
                  Null(_)
                | Bool(_)
                |  Num(_)
                |  Str(_) ⇒
                label(json.toString)
              case Arr(Vector()) ⇒ open("[]")
              case Arr(elems) ⇒
                open("[") +:
                elems
                  .mapWithIndex[VdomNode] {
                    (elem, idx) ⇒
                      Json(
                        Props(
                          elem,
                          root = false
                        ),
                        key = idx.toString
                      )
                  } :+
                close("]") toVdomArray
              case Obj(Vector()) ⇒ open("{}")
              case Obj(items) ⇒
                open("{") +:
                items
                  .map {
                    case (k, v) ⇒
                      Json(
                        Props(
                          v,
                          field = k,
                          root = false
                        ),
                        key = k
                      ): VdomNode
                  } :+
                close("}") toVdomArray
            }
          )
      }
      .build

  def apply(json: c.Json): Scala.Unmounted[Props, Unit, Unit] = apply(Props(json))
  def apply(props: Props, key: ?[String] = None): Scala.Unmounted[Props, Unit, Unit] = component.withKey(key.getOrElse("root"))(props)
}
