package org.lasersonlab.zarr

import hammerlab.option._
import hammerlab.path._
import hammerlab.str._
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.io._

case class Group(
  arrays: Map[String, Array.Ints],
  groups: Map[String, Group],
  attrs: Opt[Attrs] = None,
  metadata: Group.Metadata = Group.Metadata()
) {
  def array   (name: Str): Array.Ints           = arrays(name)
  def apply[T](name: Str): Array.S[Seq[Int], T] = arrays(name).as[T]

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

  import circe.auto._

  def apply(
    dir: Path
  ):
    Exception | Group =
    for {
      metadata ← dir.load[Metadata]
         attrs ← dir.load[Opt[Attrs]]

      arrays = Map.newBuilder[String, Array.Ints]
      groups = Map.newBuilder[String, Group]

      group ←
        dir
          .list
          .filter {
            _.basename match {
              case
                Metadata.basename |
                   Attrs.basename ⇒ false
              case _ ⇒ true
            }
          }
          .map {
            path: Path ⇒
              /** First, try to parse as an [[Array]] */
              Array.untyped(path)
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

  implicit val group: Load[Group] =
    new Load[Group] {
      override def apply(dir: Path): Exception | Group =
        Group(dir)
    }

  implicit val save: Save[Group] =
    new Save[Group] {
      def apply(t: Group, dir: Path): Throwable | Unit = {
        import cats.implicits._

        val groups =
          (
            for {
              (name, group) ← t.groups.toList
            } yield
              group.save(dir / name)
          )
          .sequence

        val arrays =
          (
            for {
              (name, array) ← t.arrays.toList
            } yield
              array.save(dir / name)
          )
          .sequence

        for {
          _ ← groups
          _ ← arrays
          _ ← t.attrs.save(dir)
          _ ← t.metadata.save(dir)
        } yield
          ()
      }
    }
}
