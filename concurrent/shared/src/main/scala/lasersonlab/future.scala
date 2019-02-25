package lasersonlab

import scala.concurrent.Future

trait future
extends org.lasersonlab.future.syntax {
  type F[+T] = Future[T]
   val F     = Future
}
object future extends future
