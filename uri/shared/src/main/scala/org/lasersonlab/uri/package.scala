package org.lasersonlab

import cats.MonadError

package object uri
extends uri.syntax {
  type MonadErr[F[_]] = MonadError[F, Throwable]
}
