package org.lasersonlab.zarr

import hammerlab.str._
import org.lasersonlab.zarr.Format._
import org.lasersonlab.zarr.io._
import org.lasersonlab.zarr.utils.Idx

import scala.concurrent.ExecutionContext

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

  sealed trait Arg[Idx]
  object Arg {
    case class Arr[Idx](k: String, v: Array.*?[Idx]) extends Arg[Idx]
    case class Grp[Idx](k: String, v: Group   [Idx]) extends Arg[Idx]
    implicit def arr[Idx: Idx.T](t: (Symbol, Array.*?[Idx])): Arg[Idx] = Arr(t._1, t._2)
    implicit def grp[Idx: Idx.T](t: (Symbol, Group   [Idx])): Arg[Idx] = Grp(t._1, t._2)
  }
  def apply[Idx: Idx.T](args: Arg[Idx]*): Group[Idx] =
    new Group(
      args.collect { case Arg.Arr(k, v) ⇒ k → v }.toMap,
      args.collect { case Arg.Grp(k, v) ⇒ k → v }.toMap
    )

  case class Metadata(
    zarr_format: Format = `2`
  )
  object Metadata {
    val basename = ".zgroup"
    implicit val _basename = Basename[Metadata](basename)
  }

  case class InvalidChild(
    path: Path,
    arrayError: Throwable,
    groupError: Throwable
  )
  extends Exception(
    s"Path $path:\nNot an array:\n$arrayError\nNot a group:\n$groupError",
    arrayError
  )

  import circe.auto._

  def apply(
    dir: Path
  )(
    implicit
    idx: Idx,
     ec: ExecutionContext
  ):
    F[Group[idx.T]] =
    for {
      metadata ← dir.load[Metadata]
         attrs ← dir.load[Option[Attrs]]

      files ←
        dir
        .children
        .map {
            _
              .filter {
                _.basename match {
                  case
                    Metadata.basename
                    |  Attrs.basename ⇒ false
                  case _ ⇒ true
                }
              }
          }

      results ←
        files
          .map {
            path: Path ⇒
              /** First, try to parse as an [[Array]] */
              (
                path,
                Array
                  .?(path)
                  .attempt
              )
          }
          .foldLeft(
            (
              List[(String, Array.*?[idx.T])](),
              List[(String, Group   [idx.T])]()
            )
            .pure[F]
          ) {
            case (results, (path, attempt)) ⇒
              attempt
                .flatMap {
                  attempt ⇒
                    results
                      .flatMap {
                        case (arrays, groups) ⇒
                          (attempt: Throwable | Array.*?[idx.T])
                            .fold(
                              (arrayError: Throwable) ⇒
                                /** If parsing as an [[Array]] failed, try parsing as a [[Group]] */
                                Group(path)
                                  .map {
                                    group ⇒
                                      (
                                        arrays,
                                        (path.basename, group) :: groups
                                      )
                                  }
                                  .attempt
                                  .map {
                                    _
                                      .left
                                      .map {
                                        groupError ⇒
                                          /** [[Array]]- and [[Group]]-parsing both failed */
                                          InvalidChild(
                                            path,
                                            arrayError,
                                            groupError
                                          ): Throwable
                                      }
                                  }
                                  .rethrow,
                              array ⇒
                                (
                                  (path.basename, array) :: arrays,
                                  groups
                                )
                                .pure[F]
                            )
                      }
                }
          }

      (arrays, groups) = results
    } yield
      Group(
        arrays.toMap,
        groups.toMap,
        attrs
      )

  implicit def group[Idx](implicit idx: Idx.T[Idx]): Load[Group[Idx]] =
    new Load[Group[Idx]] {
      def apply(dir: Path)(implicit ec: ExecutionContext): F[Group[Idx]] =
        Group(dir)
    }

  implicit def save[Idx: Idx.T]: Save[Group[Idx]] =
    new Save[Group[Idx]] {
      def direct(t: Group[Idx], dir: Path)(implicit ec: ExecutionContext): F[Unit] = {
        def groups =
          (
            for {
              (name, group) ← t.groups.toList
            } yield
              group.save(dir / name)
          )
          .sequence

        def arrays =
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
          _ ← t.   attrs.save(dir)
          // do this last, to maximize the chance that an incomplete write will result in an output directory that
          // indicates that it is not a valid Zarr group
          _ ← t.metadata.save(dir)
        } yield
          ()
      }
    }

  import hammerlab.option._
  import hammerlab.lines._
  import circe.pprint

  implicit def arrayLines[Idx]: ToLines[Array.*?[Idx]] =
    ToLines {
      _.metadata.toString  // TODO: render as JSON
    }

  implicit def mapLines[T: ToLines]: ToLines[Map[String, T]] =
    ToLines(
      map ⇒
        for {
          (k, v) ← map
        } yield
          Lines(
            k,
            indent(v)
          )
    )

  implicit def lines[Idx]: ToLines[Group[Idx]] =
    ToLines {
      case Group(
        arrays,
        groups,
        attrs,
        _
      ) ⇒
        Lines(
          attrs.map {
            attrs ⇒
              Lines(
                "attrs:",
                indent(
                  pprint(attrs.json).split("\n")
                )
              )
          },
          arrays.nonEmpty ? Lines("arrays:", indent(mapLines[Array.*?[Idx]].apply(arrays))),
          groups.nonEmpty ? Lines("groups:", indent(groups))
        )
    }
}
