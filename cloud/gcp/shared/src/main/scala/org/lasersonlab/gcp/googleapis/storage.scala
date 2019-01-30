package org.lasersonlab.gcp.googleapis

import com.softwaremill.sttp._
import io.circe.generic.auto._
import org.lasersonlab.gcp.googleapis.projects.{ Project, UserProject }
import org.lasersonlab.gcp._
import org.lasersonlab.uri._

object storage {
  val base = uri"${googleapis.base}/storage/v1"

  trait Prefix {
    def bucket: String
    def path: Vector[String]
    def fullPath = bucket +: path
    implicit def _self = this
  }

  sealed trait Path extends Prefix {
    def contents: ?[Contents]
    def basename: String
    def dirs = contents.toVector.flatMap(_.dirs)
    def objs = contents.toVector.flatMap(_.objs)
    def /(path: Vector[String]): Option[Path] = /(path.toList)
    def /(path: List[String]): Option[Path] =
      path match {
        case Nil ⇒ Some(this)
        case h :: t ⇒
          println(s"Descending: $h $t")
          contents
            .flatMap {
              case Contents(prefixes, items, _) ⇒
                prefixes
                  .flatMap {
                    _.find(_.basename == h)
                  }
                  .orElse(
                    items
                      .flatMap {
                        _.find(_.basename == h)
                      }
                  )
                  .flatMap { _ / t }
            }
      }
  }

  object Path {
    def apply(str: String, allowEmptyBasename: Boolean = false)(implicit prefix: Prefix): Vector[String] = {
      val path =
        str
          .split('/')
          .toVector ++ (
          if (str.endsWith("/") && allowEmptyBasename)
            Vector("")
          else
            Vector()
          )
      if (path.dropRight(1) != prefix.path) {
        import System.err
        err.println(path)
        err.println(prefix.path)
        throw IllegalChildPath(str, prefix)
      }
      path
    }
    def unapply(str: String): Option[(String, Vector[String])] =
      str
        .split("/") match {
          case Array(bucket, rest @ _*) ⇒
            Some(
              bucket,
              rest.toVector
            )
          case _ ⇒ None
        }
  }

  case class Dir(
    bucket: String,
    path: Vector[String],
    contents: ?[Contents] = None
  )
  extends Path
  {
    def name = (bucket +: path) mkString "/"
    def basename = path.last
    def uri = uri"${storage.base}/b/$bucket"
    def ls()(
      implicit
      config: Config,
      userProject: ?[UserProject] = None
    ):
      ?[F[Δ[Dir]]] =
    {
      contents
        .fold {
            Option(
              Http(
                uri"$uri/o?delimiter=${"/"}&prefix=${path.mkString("", "/", "/")}&userProject=$userProject"
              )
              .json[Objects]
              .map {
                objects ⇒
                  (dir: Dir) ⇒ dir.copy(contents = Some(Contents(objects)))
              }
            )
          } {
            _ ⇒ None
          }
    }
    def apply(path: List[String])(Δ: Δ[Dir]): Dir =
      path match {
        case Nil ⇒ Δ(this)
        case h :: t ⇒
          copy(
            contents =
              Some(
                contents.get(path)(Δ)
              )
          )
      }
  }
  object Dir {
    def apply(str: String)(implicit prefix: Prefix): Dir =
      Dir(
        prefix.bucket,
        Path(str)
      )
  }

  case class IllegalChildPath(str: String, prefix: Prefix) extends RuntimeException

  case class Obj(
    bucket: String,
    path: Vector[String],
    metadata: Metadata
  )
  extends Path
  {
    def name = (bucket +: path) mkString "/"
    def basename = path.last
    def contents = None
  }
  object Obj {
    def apply(metadata: Metadata)(implicit prefix: Prefix): Obj =
      Obj(
        prefix.bucket,
        Path(
          metadata.name,
          // Objects can simply end with a slash in GCS
          allowEmptyBasename = true
        ),
        metadata
      )
  }

  case class Objects(
         prefixes: ?[Vector[  String]] = None,
            items: ?[Vector[Metadata]] = None,
    nextPageToken: ?[    String ] = None
  )
  object Objects extends Kinded("storage#objects") {
    implicit val decoder = kindDecoder[Objects]
    implicit val encoder = kindEncoder[Objects]
  }

  /**
   * Application-version of [[Objects]]; [[Objects.prefixes directories]] and [[Objects.items files]] are hydrated into
   * [[Dir]]s and [[Obj]]s
   */
  case class Contents(
         prefixes: ?[Vector[Dir]] = None,
            items: ?[Vector[Obj]] = None,
    nextPageToken: ?[    String ] = None
  ) {
    def dirs: Vector[Dir] = prefixes.getOrElse(Vector())
    def objs: Vector[Obj] =    items.getOrElse(Vector())
    def apply(path: List[String])(Δ: Δ[Dir]): Contents =
      path match {
        case h :: t ⇒
          copy(
            prefixes =
              Some(
                prefixes
                  .get
                  .foldLeft { Vector[Dir]() } {
                    (prefixes, next) ⇒
                      prefixes :+ (
                        if (next.path.last == h)
                          next(t)(Δ)
                        else
                          next
                      )
                  }
              )
          )
      }
  }
  object Contents {
    def apply(objects: Objects)(implicit prefix: Prefix): Contents =
      Contents(
        objects.prefixes.map(_.map(Dir(_))),
        objects.   items.map(_.map(Obj(_))),
        objects.nextPageToken
      )
  }

  case class Billing(requesterPays: Boolean = false)
  case class Bucket(
    id: String,
    name: String,
    projectNumber: Long,
    billing: ?[ Billing] = None,
    // this doesn't exist in the Google API, but we hang it here and populate it after the fact (with subsequent
    // "list objects" requests) instead of creating separate [[Bucket]] models for the Google-API vs application
    // usage/persistence; it may be worth splitting them later to make the separation of the two concerns more explicit
    contents: ?[Contents] = None
  )
  extends Path
  {
    def bucket = name
    def basename = name
    def path = Vector[String]()
    def uri = uri"${storage.base}/b/$name"
    def ls()(
      implicit
      config: Config,
      userProject: ?[UserProject] = None
    ):
      ?[F[Δ[Bucket]]] =
    {
      contents
      .fold {
          Option(
            Http(
              uri"$uri/o?delimiter=${"/"}&userProject=$userProject"
            )
            .json[Objects]
            .map {
              objects ⇒
                (bucket: Bucket) ⇒ bucket.copy(contents = Some(Contents(objects)))
            }
          )
        } {
          _ ⇒ None
        }
    }

    def apply(path: List[String])(Δ: Δ[Dir]): Bucket =
      copy(contents = Some(this.contents.get(path)(Δ)))
  }
  object Bucket extends Kinded("storage#bucket") {
    implicit val decoder = kindDecoder[Bucket]
    implicit val encoder = kindEncoder[Bucket]
  }

  // TODO: this can probably be removed if we use a more permissive [[Decoder]] for [[Paged]], which allows the `items`
  //  [[Vector]] to be absent
  case class Buckets(
    items: ?[Vector[Bucket]],
    nextPageToken: ?[String] = None
  ) {
    def buckets = items.getOrElse(Vector())
  }
  object Buckets extends Kinded("storage#buckets") {
    implicit val decoder = kindDecoder[Buckets]
    implicit val encoder = kindEncoder[Buckets]
    implicit def unwrap(buckets: Buckets): Vector[Bucket] = buckets.buckets
  }

  def buckets()(implicit config: Config, project: Project): F[Paged[Bucket]] = {
    println(s"requesting buckets for project $project")
    Http(uri"https://www.googleapis.com/storage/v1/b?project=${project.id}".toJavaUri)
      .json[Buckets]
      .map {
        case Buckets(items, nextPageToken) ⇒
               Paged(items.getOrElse(Vector()), nextPageToken)
      }
  }
}
