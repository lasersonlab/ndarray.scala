package org.lasersonlab.ndview

import diode._
import diode.react.ReactConnector
import org.lasersonlab.gcp.googleapis.Paged
import org.lasersonlab.gcp.googleapis.projects.Project
import org.lasersonlab.gcp.googleapis.storage.{ Bucket, Dir }
import org.lasersonlab.gcp.oauth.Auth
import org.lasersonlab.ndview.model.{ Login, Logins, Projects }
import org.lasersonlab.ndview.view.Page
import org.lasersonlab.uri._

case class Model(logins: Logins)

case class NewLogin(login: Login) extends Action
case class SelectProject(id: String) extends Action
case class SelectUserProject(id: String) extends Action
case class NewProjects(id: String, projects: Paged[Project]) extends Action
case class UpdateProjects(id: String, update: Δ[Projects]) extends Action
case class UpdateBucket(loginId: String, projectId: String, id: String, Δ: Δ[Bucket]) extends Action
case class UpdateDir(loginId: String, projectId: String, dir: Dir, Δ: Δ[Dir]) extends Action
//case class NewBuckets(loginId: String, projectId: String, buckets: Paged[Bucket]) extends Action

object Circuit
  extends  diode.Circuit[Model]
     with ReactConnector[Model] {
  override protected def initialModel: Model = Model(Page.initialState)

  override protected def actionHandler: Circuit.HandlerFunction =
    ActionHandler.extractHandler(
    new ActionHandler(zoomTo(_.logins)) {
      override protected def handle: PartialFunction[Any, ActionResult[Model]] = {
        case NewLogin(login) ⇒ updated(
          value :+ login
        )
        case NewProjects(id, projects) ⇒ updated(
          value(id) { _ + projects }
        )
        case UpdateProjects(id, update) ⇒ updated(value(id) { _(update) })
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
//        case NewBuckets(loginId, projectId, buckets) ⇒ updated(
//          value.mod(loginId) {
//            login ⇒
//              login
//                .copy(
//                  projects =
//                    login
//                      .projects
//                      .mod(projectId) {
//                        _ + buckets
//                      }
//                )
//          }
//        )
      }
    }
    )
  //Circuit.zoom()

  //root
}
