package org.lasersonlab.ndview

/**
 * Wrapper for a [[org.lasersonlab.ndview.Segment.Num sequence of digits]] or
 * [[org.lasersonlab.ndview.Segment.Str any other string]].
 *
 * Split a [[String]] into such"segments" for a more intuitive sort-order than lexicographic, e.g. in the presence of
 * auto-incrementing, non-zero-padded integers in filenames.
 *
 * TODO: factor out to an file/IO-focused module
 */
sealed trait Segment

object Segment {
  case class Num(value:   Long) extends Segment
  case class Str(value: String) extends Segment

  val digits = """(\d+)""".r
  def apply(s: String): Iterable[Segment] = {
    val it =
      digits
        .findAllMatchIn(s)
        .buffered

    def str(start: Int, end: Int = s.length) =
      if (start == end)
        Iterator()
      else
        Iterator(
          Str(
            s
              .slice(
                start,
                end
              )
          )
        )

    if (!it.hasNext)
      Iterator(Str(s))
    else {
      val (lastEnd, segments) =
        it
          .foldLeft(
            (
              0,
              Iterator[Segment]()
            )
          ) {
            case ((start, segments), m) ⇒
              (
                m.end,
                segments ++
                str(
                  start,
                  m.start
                ) ++
                Iterator(
                  Num(
                    m.matched.toLong
                  )
                )
              )
          }

      segments ++ str(lastEnd)
    }
  }
  .toIterable

  implicit val ord: Ordering[Segment] =
  {
    case (Str(x), Str(y)) ⇒ x.compare(y)
    case (Num(x), Num(y)) ⇒ x.compare(y)
    case (Str(x), Num(y)) ⇒ x.toString.compare(y.toString)
    case (Num(x), Str(y)) ⇒ x.toString.compare(y.toString)
  }
}
