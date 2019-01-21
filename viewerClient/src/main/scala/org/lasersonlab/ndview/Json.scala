package org.lasersonlab.ndview

import cats.implicits._
import org.lasersonlab.uri._
import io.circe.JsonNumber
import japgolly.scalajs.react.vdom.html_<^._
import ^._
import japgolly.scalajs.react.vdom.html_<^.<._
import japgolly.scalajs.react._
import io.{ circe ⇒ c }
import japgolly.scalajs.react.component.Scala

object Json {
  object Null { def unapply(json: c.Json): Option[                    Unit    ] = json.asNull                   }
  object  Num { def unapply(json: c.Json): Option[              JsonNumber    ] = json.asNumber                 }
  object  Str { def unapply(json: c.Json): Option[                  String    ] = json.asString                 }
  object Bool { def unapply(json: c.Json): Option[                 Boolean    ] = json.asBoolean                }
  object  Arr { def unapply(json: c.Json): Option[ Vector[          c.Json  ] ] = json.asArray                  }
  object  Obj { def unapply(json: c.Json): Option[ Vector[ (String, c.Json) ] ] = json.asObject.map(_.toVector) }

  case class Props(
    json: c.Json,
    key: ?[String] = None,
    field: ?[String] = None,
    root: Boolean = true
  )

  def  open(str: String): VdomNode = div(key :=  "open")(label(str))
  def close(str: String): VdomNode = div(key := "close")(str)

  val component =
    ScalaComponent
      .builder[Props]("Json")
      .render_P {
        case props @ Props(json, _, _, root) ⇒
          def label(str: String): VdomNode =
            props
              .field
              .fold {
                str
              } {
                field ⇒ s"$field: $str"
              }

          div(
            key := props.key.getOrElse(""),
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
                  .mapWithIndex {
                    (elem, idx) ⇒
                      Json(
                        Props(
                          elem,
                          key = idx.toString,
                          root = false
                        )
                      ): VdomNode
                      //.withKey(idx.toString)
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
                          key = k,
                          field = k,
                          root = false
                        )
                      ): VdomNode
                      //.withKey(k)
                  } :+
                close("}") toVdomArray
            }
          )
      }
      .build

  def apply(props: Props): Scala.Unmounted[Props, Unit, Unit] = component(props)
}
