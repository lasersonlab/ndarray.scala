package org.lasersonlab.ndview

import cats.Applicative
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^.^._
import japgolly.scalajs.react.vdom.html_<^._
import lasersonlab.diode.Proxy
import org.lasersonlab.gcp.SignIn.Recover
import org.lasersonlab.gcp.oauth.{ Auth, Params }

package object view {
  type Router = RouterCtl[Route]
  def cls(k: String, c: String = null) =
    TagMod(
      key := k,
      className := Option(c).getOrElse(k)
    )

  implicit def failAuth[F[_]](
    implicit
    proxy: Proxy,
    auth: Auth,
    F: Applicative[F],
  ): Recover[F] =
    new Recover[F] {
      override def apply()(implicit auth: Auth, params: Params): F[Unit] =
        F.pure { proxy.dispatchNow(FailAuth(auth)) }
    }
}
