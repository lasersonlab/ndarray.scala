package lasersonlab

trait threads {
  implicit case object NumThreads { val value = 1 }
  type NumThreads = NumThreads.type
  implicit def unwrapNumThreads(numThreads: NumThreads): Int = numThreads.value
}
object threads extends threads
