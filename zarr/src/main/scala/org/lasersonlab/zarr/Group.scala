package org.lasersonlab.zarr

import java.io.IOException
import java.nio.file.{ Files, StandardCopyOption }
import java.nio.file.Files.createTempDirectory

import hammerlab.option._
import hammerlab.str._
import org.hammerlab.paths.Path
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.io._
import org.lasersonlab.zarr.utils.Idx

import scala.util.Try

case class Group[Idx](
  arrays: Map[String, Array.*?[Idx]] =      Map.empty[String, Array.*?[Idx]],
  groups: Map[String, Group   [Idx]] =      Map.empty[String, Group   [Idx]],
  attrs:              Option[Attrs]  =           None,
  metadata:          Group.Metadata = Group.Metadata()
)(
  implicit
  idx: Idx.T[Idx]
) {
  def     !   (name: Str): Array.*?[      Idx   ] = arrays(name)
  def array   (name: Str): Array.*?[      Idx   ] = arrays(name)
  def apply[T](name: Str): Array.Of[List, Idx, T] = arrays(name).as[T]
  def     →[T](name: Str): Array.Of[List, Idx, T] = arrays(name).as[T]

  def     /(name: Str): Group[Idx] = groups(name)
  def group(name: Str): Group[Idx] = groups(name)
}

object Group {
  case class Metadata(
    zarr_format: Format = `2`
  )
  object Metadata {
    val basename = ".zgroup"
    implicit val _basename = Basename[Metadata](basename)
  }

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

  def apply[Idx](
    dir: Path
  )(
    implicit
    idx: Idx.T[Idx]
  ):
    Exception | Group[Idx] =
    for {
      metadata ← dir.load[Metadata]
         attrs ← dir.load[Option[Attrs]]

      arrays = Map.newBuilder[String, Array.*?[Idx]]
      groups = Map.newBuilder[String, Group   [Idx]]

      files ←
        try {
          Right(
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
          )
        } catch {
          case e: IOException ⇒ Left(e)
        }

      group ←
        files
          .map {
            path: Path ⇒
              /** First, try to parse as an [[Array]] */
              Array.?(path)
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
          .sequence
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

  implicit def group[Idx](implicit idx: Idx.T[Idx]): Load[Group[Idx]] =
    new Load[Group[Idx]] {
      def apply(dir: Path): Exception | Group[Idx] =
        Group(dir)
    }

  implicit def save[Idx: Idx.T]: Save[Group[Idx]] =
    new Save[Group[Idx]] {
      def apply(t: Group[Idx], dir: Path): Throwable | Unit = {
        val tmp = Path(createTempDirectory(s"tmp-${dir.basename}"))
        val groups =
          (
            for {
              (name, group) ← t.groups.toList
            } yield
              group.save(tmp / name)
          )
          .sequence

        val arrays =
          (
            for {
              (name, array) ← t.arrays.toList
            } yield
              array.aux.save(tmp / name)
          )
          .sequence

        for {
          _ ← groups
          _ ← arrays
          _ ← t.   attrs.save(tmp)
          _ ← t.metadata.save(tmp)
          _ ←
            Try {
              import StandardCopyOption._
              Files.move(tmp, dir, REPLACE_EXISTING, ATOMIC_MOVE)
            }
            .toEither
        } yield
          ()
      }
    }
}
