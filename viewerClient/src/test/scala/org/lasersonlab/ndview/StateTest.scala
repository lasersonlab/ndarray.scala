package org.lasersonlab.ndview

import cats.implicits._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import lasersonlab.opt.circe._
import org.lasersonlab.gcp.Metadata
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage.{ Billing, Bucket, Buckets, Dir, Objects }
import org.lasersonlab.gcp.googleapis._
import org.lasersonlab.gcp.oauth.{ Auth, Params, RedirectUrl, Scopes, _ }
import org.lasersonlab.ndview.model.{ Login, Logins, Projects }
import org.lasersonlab.ndview.view.Page.pprint
import org.lasersonlab.test.future.Assert
import utest._
import org.lasersonlab.circe.SingletonCodec._
import org.lasersonlab.files.Local

import scala.concurrent.Future

// Need this to take precedence over Encoder.encodeIterable
import org.lasersonlab.gcp.googleapis.Paged.pagedEncoder

object StateTest
  extends lasersonlab.Suite
     with Assert.syntax
{

  override lazy val resourceDirectories: List[Path] = Local("viewerClient/src/test/resources") :: Nil

  val tests = Tests {
//    'encode - {
//      val actual = pprint(logins.asJson)
//      ==(
//        actual,
//        json
//      )
//      .onError {
//        case _ ⇒
//          Local("actual").write(actual)
//          Local("expected").write(json)
//          ().pure[Future]
//      }
//    }
//    'decode -
//      ==(
//        decode[Logins](json),
//        Right(logins)
//      )

    import lasersonlab.circe._

    'test - {
      val actual = decode[Foo]("""{ "a": true, "c": true }""")
      println(actual)
      assert(
        actual ==
        Right(Foo(true, None))
      )
    }

//    'dirs - {
//      resource("dirs.json")
//        .string
//        .map {
//          json ⇒
//            val dirs = decode[?[Vector[Dir]]](json)
////            val dirs = decode[Vector[Dir]](json)
//            println(dirs)
//            assert(
//              dirs == Right(Vector())
//            )
//        }
//    }
//
//    'objects - {
//      for {
//        in ← resource("objects.json").string
//        out ← resource("objects-out.json").string
//        objects = decode[Objects](in)
//      } yield {
//        assert(
//          objects.map {
//            o ⇒
//              val str = pprint(o.asJson)
//              //Local("actual").write(str)
//              str
//          } == Right(out)
//        )
//        ()
//      }
//    }
  }

  case class Foo(a: Boolean, b: Option[Boolean], c: Boolean = false, d: Option[Boolean] = None)
}
