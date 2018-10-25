package hammerlab.path

import java.io.{ InputStream, OutputStream }
import java.net.URI

import hammerlab.str._

case class Path(uri: URI) {
  def /(t: Str): Path = ???
  def basename: String = ???
  def exists: Boolean = ???
  def list: Iterator[Path] = ???
  def mkdirs: Unit = ???
  def write(s: String): Unit = ???
  def inputStream: InputStream = ???
  def outputStream(mkdirs: Boolean = false): OutputStream = ???
  def read: String = ???
  def readBytes: Array[Byte] = ???
}
object Path {
  def apply(uri: String): Path = ???
}
