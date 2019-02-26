package org.lasersonlab.zarr.cmp

import java.net.URI

import cats.data.{ Ior, Nested, NonEmptyList }
import cats.implicits._
import hammerlab.lines._
import hammerlab.indent.spaces2
import hammerlab.option._
import hammerlab.or._
import lasersonlab.zarr.{ F, Path }
import org.lasersonlab.test.future.{ CanEq, Cmp }
import org.lasersonlab.files.Local
import Ior.fromOptions
import NonEmptyList.fromList

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

object path
  extends CanEq.syntax {

  case class Diff[FileDiff](
    l: Path,
    r: Path,
    diff: LocalDiff[FileDiff]
  )(implicit val lines: ToLines[FileDiff]) {
    override def toString: String = this.showLines
  }
  object Diff {
    implicit def lines[T]: ToLines[Diff[T]] = {
      d: Diff[T] ⇒ import d._
        implicit val _lines = d.lines
        Lines(
          s"'Expected' path: $l",
          s"'Actual' path: $r",
          diff match {
            case L(L(_)) ⇒ diff.toString
            case L(R(fileDiff)) ⇒ d.lines(fileDiff)
            case R(dirDiff) ⇒ dirDiff.lines
          }
        )
    }
  }

  type LocalDiff[FileDiff] =
     TypeDiff ||
     FileDiff ||
      DirDiff [FileDiff]

  sealed trait TypeDiff extends Product with Serializable

  case object   ExtraFile extends TypeDiff
  case object MissingFile extends TypeDiff

  case object   ExpectedDir extends TypeDiff
  case object UnexpectedDir extends TypeDiff

  case class DirDiff[FileDiff: ToLines](diffs: NonEmptyList[(URI, LocalDiff[FileDiff])]) {
    def typeDiffs = diffs.collect { case (uri, L(L(diff))) ⇒ (uri, diff) }.sortBy { _._1 }
    def fileDiffs = diffs.collect { case (uri, L(R(diff))) ⇒ (uri, diff) }.sortBy { _._1 }
    def  dirDiffs = diffs.collect { case (uri,   R(diff))  ⇒ (uri, diff) }.sortBy { _._1 }

    override def toString: String = this.showLines
  }
  object DirDiff {
    implicit def lines[T: ToLines]: ToLines[DirDiff[T]] = {
      diffs ⇒
        import diffs._
        Lines(
          typeDiffs.nonEmpty ?
          Lines(
            typeDiffs.map { case (path, diff) ⇒ s"$path: $diff" }
          ),
          fileDiffs.nonEmpty ?
          Lines(
            fileDiffs.map {
              case (path, diff) ⇒
                val lines = diff.lines
                lines match {
                  case Lines(line #:: Stream()) ⇒ println(s"$path, single: "); println(line); Lines(s"$path: $line")
                  case lines ⇒ println(s"$path: ${lines.size}"); printlns(lines); println("done"); Lines(s"$path:", lines)
                }
            }
          ),
          dirDiffs.nonEmpty ?
          Lines(
            dirDiffs.map {
              case (path, diff) ⇒
                Lines(
                  s"$path:",
                  indent(diff.lines)
                )
            }
          )
        )
    }
  }

  def children(path: Path)(implicit ec: ExecutionContext): F[Map[URI, Path]] =
    path
      .list
      .map {
        _
          .map {
            child ⇒
              path
                .uri
                .relativize(
                  child.uri
                ) →
              child
          }
          .toMap
      }

}

trait path {
  import path.{ Diff ⇒ _, _ }

  implicit def localCmp(implicit ec: ExecutionContext, str: Cmp[String]): Cmp.Aux[Local, path.Diff[Lines]] = pathCmp.map(l ⇒ l, r ⇒ r)
  implicit def pathCmp(
    implicit
    ec: ExecutionContext,
    str: Cmp[String]
  ):
    Cmp.Aux[Path, path.Diff[Lines]] =
    new CanEq[Path, Path] {
      type FileDiff = Lines
      type LocalDiff = path.LocalDiff[FileDiff]
      type DirDiff = path.DirDiff[FileDiff]
      type Diff = path.Diff[FileDiff]
      def apply(l: Path, r: Path): F[?[Diff]] =
        (l.isDirectory, r.isDirectory) match {
          case ( true,  true) ⇒
            type Entry = (URI, LocalDiff)
            Nested(
              for {
                 leftMap ← children(l)
                rightMap ← children(r)
                 extra =
                   for {
                     (k, lv) ← leftMap.toList
                     if !rightMap.contains(k)
                   } yield
                     k → L(L(ExtraFile))
                missing =
                  for {
                    (k, rv) ← rightMap.toList
                    if !leftMap.contains(k)
                  } yield
                    k → L(L(MissingFile))
                both ←
                  {
                    for {
                      (k, lv) ← leftMap.toList
                      rv ← rightMap.get(k)
                    } yield
                      Nested(apply(lv, rv))
                        .map {
                          case path.Diff(_, _, diff) ⇒
                            k → diff
                        }
                        .value
                  }
                  .sequence
                  .map {
                    _
                      .collect {
                        case Some((k, dirDiff)) ⇒ k → dirDiff
                      }
                  }
                diffs = extra ++ missing ++ both
              } yield
                fromList(diffs).map(DirDiff(_))
            )
            .map { dirDiff ⇒ path.Diff(l, r, R(dirDiff)) }
            .value
          case (false, false) ⇒
            for {
               left ← l.read
              right ← r.read
            } yield
              !(left sameElements right) ? {
                val lstr =  left.forall(_.isValidChar) ? (new String(  left, "UTF-8")) map { _.split("\n") }
                val rstr = right.forall(_.isValidChar) ? (new String( right, "UTF-8")) map { _.split("\n") }
                path.Diff(
                  l, r,
                  L(R(
                    (lstr, rstr) match {
                      case (Some(left), Some(right)) ⇒
                        Lines(
                          "Expected:",
                          indent(right),
                          "Actual:",
                          indent( left),
                        )
                      case (None, None) ⇒ Lines("binary files differ")
                      case (Some(left), None) ⇒ Lines("Expected binary, actual text")
                      case (None, Some(_)) ⇒ Lines("Expected text, got binary data")
                    }
                  ))
                )
              }
          case ( true, false) ⇒ F { Some(path.Diff(l, r, L(L(UnexpectedDir)))) }
          case (false,  true) ⇒ F { Some(path.Diff(l, r, L(L(  ExpectedDir)))) }
        }
    }
}
