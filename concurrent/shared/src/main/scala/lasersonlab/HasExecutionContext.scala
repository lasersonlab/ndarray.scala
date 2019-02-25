package lasersonlab

import scala.concurrent.ExecutionContext

trait HasExecutionContext {
  implicit def ec: ExecutionContext
}
