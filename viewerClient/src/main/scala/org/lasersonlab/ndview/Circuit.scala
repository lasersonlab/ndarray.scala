package org.lasersonlab.ndview

import diode._
import diode.react.ReactConnector
import org.lasersonlab.gcp.googleapis.{ Paged, storage }
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage.{ Bucket, Dir }
import org.lasersonlab.gcp.oauth.Auth
import org.lasersonlab.ndview.model.{ Login, Logins, Projects }
import org.lasersonlab.uri._

case class Model(
  logins: Logins = Logins(),
  route: Route = Vector(),
  closedFolders: ClosedFolders = ClosedFolders()
)

case class          NewLogin(  login:  Login)                                              extends Action
case class       ExpireLogin(  login:  Login)                                              extends Action
case class         FailLogin(  login:  Login)                                              extends Action
case class         FailAuth (   auth:   Auth)                                              extends Action
case class     SelectProject(     id: String)                                              extends Action
case class SelectUserProject(     id: String)                                              extends Action
case class       NewProjects(     id: String, projects: Paged[Project])                    extends Action
case class    UpdateProjects(     id: String, update: Δ[Projects])                         extends Action
case class      UpdateBucket(loginId: String, projectId: String, id: String, Δ: Δ[Bucket]) extends Action
case class         UpdateDir(loginId: String, projectId: String, dir: Dir, Δ: Δ[Dir])      extends Action
case class  OpenDir(path: storage.Path) extends Action
case class CloseDir(path: storage.Path) extends Action
case object Clear extends Action

case class Circuit(initialModel: Model)(implicit httpConfig: http.Config)
  extends  diode.Circuit[Model]
     with ReactConnector[Model] {
  override protected def actionHandler: HandlerFunction =
    composeHandlers(
      modelHandler,
      loginsHandler,
      treeHandler
    )

  val modelHandler =
    new ActionHandler[Model, Model](new RootModelRW(initialModel)) {
      override protected def handle: PartialFunction[Any, ActionResult[Model]] = {
        case Clear ⇒
          updated(Model())
      }
    }

  val loginsHandler =
    new ActionHandler(zoomTo(_.logins)) {
      override protected def handle: PartialFunction[Any, ActionResult[Model]] = {
        case NewLogin(login) ⇒
          println("action: NewLogin")
          implicit val auth = login.auth
          login
            .projects
            .fetchBuckets
            .map {
              ΔF ⇒
                ΔF.map {
                  Δ ⇒
                    UpdateProjects(login.id, Δ)
                }
            }
            .map { Effect(_) }
            .fold {
              updated(value :+ login)
            } {
              updated(value :+ login, _)
            }
        case NewProjects(id, projects) ⇒ updated(
          value(id) { _ + projects }
        )
        case UpdateProjects(id, update) ⇒
          val next = value(id) { _(update) }
          println(s"Updating projects to $next")
          updated(next)
        case     SelectProject(id) ⇒ updated(value(_.   project(id)))
        case SelectUserProject(id) ⇒ updated(value(_.userProject(id)))
        case UpdateBucket(loginId, projectId, id, fn) ⇒
          updated(
            value(loginId) {
              _(projectId) {
                _(id)(fn)
              }
            }
          )
        case UpdateDir(loginId, projectId, Dir(bucket, path, objects), fn) ⇒
          updated(
            value(loginId) {
              _(projectId) {
                _(bucket) {
                  _(path.toList)(fn)
                }
              }
            }
          )
        case ExpireLogin(expire) ⇒
          println(s"Expiring login: $expire")
          newLogin(
            for {
              login ← value.get(expire.id)
              if expire.auth == login.auth
              auth ← login.expire
            } yield
              (login.id, auth)
          )
        case FailLogin(failed) ⇒
          println(s"Failing login: $failed")
          newLogin(
            for {
              login ← value.get(failed.id)
              if failed.auth == login.auth
              auth ← login.fail
            } yield
              (login.id, auth)
          )
        case FailAuth(failed) ⇒
          newLogin(
            for {
              login ← value.find(_.auth.token == failed.token)
              auth ← login.auth.fail
            } yield
              (login.id, auth)
          )
      }

    def newLogin(auth: Option[(String, Auth)]) =
      auth
        .fold { noChange } {
          case (id, auth) ⇒
            updated(
              value(id) { _.copy(auth = auth) }
            )
        }
    }

  val treeHandler =
    new ActionHandler(zoomTo(_.closedFolders)) {
      override protected def handle: PartialFunction[Any, ActionResult[Model]] = {
        case  OpenDir(path) ⇒ updated(value.clear(path.fullPath      ))
        case CloseDir(path) ⇒ println(s"close circuit: $path"); updated(value.  set(path.fullPath, true))
      }
    }
}
