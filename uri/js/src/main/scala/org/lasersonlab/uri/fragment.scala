package org.lasersonlab.uri

import org.scalajs.dom.document
import java.net.URLDecoder.decode

import scala.util.matching.Regex.Groups

object fragment {
  val regex = """([^&=]+)=([^&]*)""".r
  def apply() = document.location.hash.drop(1)
  def map: Map[String, String] =
    regex
      .findAllMatchIn(fragment())
      .map {
        case Groups(k, v) ⇒
          decode(k, "UTF-8") →
          decode(v, "UTF-8")
      }
      .toMap
}
