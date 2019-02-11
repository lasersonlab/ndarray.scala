package lasersonlab

trait threads {
  implicit case object NumThreads { val value = 1 }
  type NumThreads = NumThreads.type
  implicit def unwrapNumThreads(numThreads: NumThreads): Int = numThreads.value

  object pool {
    implicit val default = scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
  }
}
object threads extends threads
