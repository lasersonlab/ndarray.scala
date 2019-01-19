package org.lasersonlab

import cats.MonadError
import hammerlab.option.Opt

package object uri
extends uri.syntax
   with lasersonlab.future
   with lasersonlab.opt.circe {
  type MonadErr[F[_]] = MonadError[F, Throwable]
  type ?[+T] = Opt[T]
}
