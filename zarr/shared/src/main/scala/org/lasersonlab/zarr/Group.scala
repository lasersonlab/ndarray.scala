package org.lasersonlab.zarr

import io.circe._
import parser._
import generic.auto._
import Format._
import cats.data.Ior
import cats.data.Ior.Both
import hammerlab.path._
import hammerlab.str._

case class Group(
  arrays: Map[String, untyped.Array],
  groups: Map[String, Group]
) {
  def array(name: Str): untyped.Array = arrays(name)
  def group(name: Str):         Group = groups(name)
}

object Group {
  case class Metadata(
    zarr_format: Format = `2`
  )
  object Metadata {
    val basename = ".zgroup"
    def apply(dir: Path): Exception | Metadata =
      dir ? basename flatMap {
        path ⇒
          decode[Metadata](path.read)
      }
  }

  import cats.implicits._

  case class InvalidChild(
    path: Path,
    arrayError: Exception,
    groupError: Exception
  )
  extends Exception(
    s"Path $path:\nNot an array:\n$arrayError\nNot a group:\n$groupError",
    arrayError
  )

  def apply(
    dir: Path
  ):
    Exception | Group =
    for {
      metadata ← Metadata(dir)
      arrays = Map.newBuilder[String, untyped.Array]
      groups = Map.newBuilder[String,         Group]
      group ←
        dir
          .list
          .filter { _.basename != Metadata.basename }
          .map {
            path: Path ⇒
              untyped.Array(path)
                .fold(
                  arrayError ⇒
                    Group(path)
                      .map {
                        groups +=
                          path.basename → _
                      }
                    .left
                    .map {
                      groupError ⇒
                        InvalidChild(
                          path,
                          arrayError,
                          groupError
                        )
                    },
                  array ⇒
                    Right(
                      arrays +=
                        path.basename → array
                    )
                )
          }
          .toList
          .sequence[Either[Exception, ?], Any]
          .map {
            a ⇒
              Group(
                arrays.result,
                groups.result
              )
          }
    } yield
      group
}
