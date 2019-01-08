package org.lasersonlab.uri.gcp

import cats.effect._
import cats.implicits._
import com.softwaremill.sttp._
import io.circe.Decoder.Result
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.auto._
import io.circe.{ Decoder, DecodingFailure, HCursor }
import org.lasersonlab.uri.Http
import shapeless.Lazy

object googleapis {
  val base = uri"https://www.googleapis.com"

  def kindDecoder[A](kind: String)(implicit d: Lazy[DerivedDecoder[A]]): Decoder[A] =
    new Decoder[A] {
      override def apply(c: HCursor): Result[A] =
        c
          .downField("kind")
          .as[String]
            .flatMap {
              case k if k == kind ⇒ d.value(c)
              case k ⇒ Left(DecodingFailure(s"Found kind $k, expected $kind", c.history))
            }
    }

  object storage {
    val base = uri"${googleapis.base}/storage/v1"

    case class Objects(
      prefixes: Option[Seq[String]] = None,
      items: Option[Seq[Metadata]] = None,
      nextPageToken: Option[String] = None
    )
    object Objects {
      implicit val decoder = kindDecoder[Objects]("storage#objects")
    }

    case class Billing(requesterPays: Boolean = false)
    case class Bucket(
      id: String,
      name: String,
      projectNumber: Long,
      billing: Billing
    )
    object Bucket {
      implicit val decoder = kindDecoder[Bucket]("storage#bucket")
    }
    case class Buckets(items: Seq[Bucket])
    object Buckets {
      implicit val decoder = kindDecoder[Buckets]("storage#buckets")
    }

    def buckets[F[_]: ConcurrentEffect](implicit auth: Auth, project: Project) =
      Http(uri"https://www.googleapis.com/storage/v1/b?project=$project&userProject=$project".toJavaUri)
        .json[Buckets]
        .map {
          case Buckets(items) ⇒
            items
            .toList
            .map {
              b ⇒
                GCS[F](b.name)
            }
        }
    //    object buckets {
//      val base = s"${storage.base}/b"
//      def list(
//        project: String,
//        userProject: Option[String] = None
//      )(
//        implicit
//        auth: Auth
//      ) = {
//        Ajax.get(
//          uri"${buckets.base}?project=$project&userProject=$userProject".toString,
//          headers =
//            Map(
//              "Authorization" → s"Bearer ${auth.token}"
//            )
//        )
//        .transformWith {
//          case Failure(e) ⇒
//            Future(
//              e match {
//                case AjaxException(xhr) ⇒
//                  xhr.status match {
//                    case  401 ⇒
//                      println("received 401; sign in again…")
//                      Left(e)
//                    case code ⇒ Left(e)
//                  }
//                case _ ⇒ Left(e)
//              }
//            )
//          case Success(r) ⇒
//            import io.circe.parser
//            Future(parser.decode[Buckets](r.responseText))
//        }
//      }
//    }
  }
}
