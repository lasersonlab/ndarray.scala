package org.lasersonlab.ndview

import cats.implicits._
import hammerlab.option.Non
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
import org.lasersonlab.gcp.googleapis.storage.Dir.Repr
import org.lasersonlab.uri.Local

import scala.concurrent.Future

// Need this to take precedence over Encoder.encodeIterable
import org.lasersonlab.gcp.googleapis.Paged.pagedEncoder

object StateTest
  extends lasersonlab.Suite
     with Assert.syntax
{

  override lazy val resourceDirectories: List[Path] = Local("viewerClient/src/test/resources") :: Nil
  /*

    val json = resource("state.json").string

    val logins =
      Logins(
        Vector(
          Login(
            Auth(
              "ya29.GlyZBm2eJCPfrAeVndkS9nfdKNEvZ1OANkqdfORGUbxZcQiSAhx0KviK63SFnLD_Re2Xk_TEbHoVbQLNGjyl9CKlKPgVRH7zfDZiirfE344wPr7prKrTOodjQRJLmA",
              1548125206,
              List(
                "email profile https://www.googleapis.com/auth/cloud-platform.read-only https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/devstorage.read_only"
              ),
              Params(
                ClientId("218219996328-lltra1ss5e34hlraupaalrr6f56qmiat.apps.googleusercontent.com"),
                RedirectUrl("http://localhost:8000"),
                Scopes(
                  Scope("https://www.googleapis.com/auth/userinfo.email"),
                  Scope("https://www.googleapis.com/auth/userinfo.profile"),
                  Scope("https://www.googleapis.com/auth/devstorage.read_only"),
                  Scope("https://www.googleapis.com/auth/cloud-platform.read-only"),
                )
              )
            ),
            User(
              "106937999264733402135",
              "Ryan Williams",
              Some("ryan@lasersonlab.org"),
              "https://lh6.googleusercontent.com/-A5rE5wsEUIM/AAAAAAAAAAI/AAAAAAAAAAc/ig9En0FXVBs/photo.jpg"
            ),
            Projects(
              Paged(
                Vector(
                  Project(
                    "hca-scale",
                    "hca-scale",
                    "218219996328",
                    Some(
                      Paged(
                        Vector(
                          Bucket(
                            "dataproc-ec0dd18c-8914-432f-9e13-8335beaa47db-us-east1",
                            "dataproc-ec0dd18c-8914-432f-9e13-8335beaa47db-us-east1",
                            218219996328L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-dataproc-initialization-actions",
                            "ll-dataproc-initialization-actions",
                            218219996328L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-sc-data",
                            "ll-sc-data",
                            218219996328L,
                            Some(Billing(true)),
                            Some(
                              Objects(
                                Some(
                                  Vector(
                                    "10x/",
                                    "hca/",
                                    "loom/",
                                    "mca/"
                                  )
                                ),
                                Some(
                                  Vector(
                                    Metadata(
                                      "ll-sc-data/test-public.txt/1547143025743385",
                                      "test-public.txt",
                                      11,
                                      "cF8BqGDbI/o0qsRrM5VIXw=="
                                    ),
                                    Metadata(
                                      "ll-sc-data/test-write.txt/1532480791118244",
                                      "test-write.txt",
                                      11,
                                      "nHL1vwsRP2XD6kTmQg85lw=="
                                    ),
                                    Metadata(
                                      "ll-sc-data/test.txt/1532480352143794",
                                      "test.txt",
                                      11,
                                      "cF8BqGDbI/o0qsRrM5VIXw=="
                                    )
                                  )
                                ),
                                None
                              )
                            )
                          ),
                          Bucket(
                            "ll-sc-data-bkup",
                            "ll-sc-data-bkup",
                            218219996328L,
                            None,
                            Some(
                              Objects(
                                Some(
                                  Vector("10x/")
                                ),
                                None,
                                None
                              )
                            )
                          ),
                          Bucket(
                            "ll-sc-scripts",
                            "ll-sc-scripts",
                            218219996328L,
                            None,
                            None
                          )
                        ),
                        None
                      )
                    )
                  ),
                  Project(
                    "Laserson Lab",
                    "laserson-lab",
                    "339088619166",
                    Some(
                      Paged(
                        Vector(
                          Bucket(
                            "ll-adhoc",
                            "ll-adhoc",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-airr-seq",
                            "ll-airr-seq",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-dropbox-larman",
                            "ll-dropbox-larman",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-dropbox-tomasz",
                            "ll-dropbox-tomasz",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-hobbes",
                            "ll-hobbes",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-nat-prot-ex-data",
                            "ll-nat-prot-ex-data",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-phage-libraries-private",
                            "ll-phage-libraries-private",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-phip-analysis",
                            "ll-phip-analysis",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-phip-seq",
                            "ll-phip-seq",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-raw-seq",
                            "ll-raw-seq",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-scratch",
                            "ll-scratch",
                            339088619166L,
                            None,
                            None
                          ),
                          Bucket(
                            "ll-seq",
                            "ll-seq",
                            339088619166L,
                            None,
                            None
                          )
                        ),
                        None
                      )
                    )
                  )
                ),
                None
              ),
              Some("hca-scale")
            ),
            Non
          )
        ),
        Some("106937999264733402135")
      )

  */
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

    'dir - {
      println(decode[Either[String, Repr]]("\"yay\""))
    }

    'dirs - {
      resource("dirs.json")
        .string
        .map {
          json ⇒
            val dirs = decode[?[Vector[Dir]]](json)
//            val dirs = decode[Vector[Dir]](json)
            println(dirs)
            assert(
              dirs == Right(Vector())
            )
        }
    }

    'objects - {
      for {
        in ← resource("objects.json").string
        out ← resource("objects-out.json").string
        objects = decode[Objects](in)
      } yield {
        assert(
          objects.map {
            o ⇒
              val str = pprint(o.asJson)
              //Local("actual").write(str)
              str
          } == Right(out)
        )
        ()
      }
    }
  }

}
