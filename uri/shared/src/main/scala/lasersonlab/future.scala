package lasersonlab

import scala.concurrent.Future

trait future {
  type F[+T] = Future[T]
   val F     = Future
}
object future extends future
