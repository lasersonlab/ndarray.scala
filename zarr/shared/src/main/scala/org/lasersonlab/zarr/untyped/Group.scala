package org.lasersonlab.zarr.untyped

import hammerlab.option._
import hammerlab.path._
import hammerlab.str._
import io.circe.generic.auto._
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr._
import org.lasersonlab.zarr.group.{ Basename, Load }
import org.lasersonlab.zarr.group.Load.Ops

case class Group(
  arrays: Map[String, Array],
  groups: Map[String, Group],
  attrs: Opt[Attrs] = None,
  metadata: Group.Metadata = Group.Metadata()
) {
  def array(name: Str): Array = arrays(name)
  def group(name: Str): Group = groups(name)
}

object Group {
  case class Metadata(
    zarr_format: Format = `2`
  )
  object Metadata {
    val basename = ".zgroup"
    implicit val _basename = Basename[Metadata](basename)
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
      metadata ← dir.load[Metadata]
         attrs ← dir.load[Opt[Attrs]]

      arrays = Map.newBuilder[String, Array]
      groups = Map.newBuilder[String, Group]

      group ←
        dir
          .list
          .filter {
            _.basename != Metadata.basename
          }
          .map {
            path: Path ⇒
              /** First, try to parse as an [[Array]] */
              Array(path)
                .fold(
                  arrayError ⇒
                    /** If that failed, parse as a [[Group]] */
                    Group(path)
                      .map {
                        groups +=
                          path.basename → _
                      }
                    .left
                    .map {
                      groupError ⇒
                        /** [[Array]]- and [[Group]]-parsing both failed */
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
          /**
           * transpose the per-child [[Either]]s out; the elements themselves were type-less; we accreted [[Group]]s
           * and [[Array]]s into the builders ([[arrays]], [[groups]]) along the way
           */
          .sequence[Either[Exception, ?], Any]
          .map {
            a ⇒
              Group(
                arrays.result,
                groups.result,
                attrs
              )
          }
    } yield
      group
}
