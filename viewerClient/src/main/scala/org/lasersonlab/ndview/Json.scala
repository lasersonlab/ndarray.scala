package org.lasersonlab.ndview

import cats.implicits._
import org.lasersonlab.uri._
import io.circe.JsonNumber
import slinky.core.Component
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._
import io.{ circe ⇒ c }

@react class Json extends Component {
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

  type State = Unit

  val initialState: Unit = ()

  def label(str: String) =
    props
      .field
      .fold {
        str
      } {
        field ⇒ s"$field: $str"
      }

  def  open(str: String) = div(key :=  "open")(label(str))
  def close(str: String) = div(key := "close")(str)

  def render(): ReactElement = {
    val Props(json, _, _, root) = props
    div(
      key := props.key.getOrElse(""),
      className := s"json${if (root) "" else " indent"}"
    )(
      json match {
        case
            Null(_)
          | Bool(_)
          |  Num(_)
          |  Str(_) ⇒
          Seq(label(json.toString): ReactElement)
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
                )
                .withKey(idx.toString): ReactElement
            } :+
          close("]")
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
                )
                .withKey(k): ReactElement
            } :+
          close("}")
      }
    )
  }
}
