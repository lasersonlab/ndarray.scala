package org.lasersonlab.ndview.view

import java.lang.System.err

import io.circe.Decoder
import org.scalajs.dom.window.localStorage
import io.circe.generic.auto._
import io.circe.parser.decode

object LocalStorage {
  val stateKey = "app-state"
  def apply[T: Decoder](default: ⇒ T): T = {
    val str = localStorage.getItem(stateKey)
    Option(str)
      .fold {
        default
      } {
        decode[T](_) match {
          case Left(e) ⇒
            err.println(s"Failed to parse state from localStorage:")
            err.println(e)
            err.println(str)
            localStorage.removeItem(stateKey)
            default
          case Right(state) ⇒ state
        }
      }
  }
}
