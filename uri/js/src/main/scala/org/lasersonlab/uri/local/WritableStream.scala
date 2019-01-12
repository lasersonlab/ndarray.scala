package org.lasersonlab.uri.local

import lasersonlab.future.F
import java.io.{ IOException, OutputStream }

import org.lasersonlab.uri.local.WritableStream._

import scala.collection.mutable
import scala.concurrent.Promise
import org.lasersonlab.uri.Local.fs
import slogging.LazyLogging

case class WritableStream(str: String)
  extends OutputStream
     with LazyLogging
{
  val stream = fs.createWriteStream(str)
  stream.cork()
  var open = true
  val queue = mutable.Queue[Array[Byte]]()
  var closed = false
  val closedPromise = Promise[Unit]()
  stream.on(
    "drain",
    {
      _: Any ⇒
        val backlog = queue.size
        logger.info(s"stream drained (was open? $open; $backlog buffers)")
        open = true
        while (open && queue.nonEmpty) {
          write(queue.dequeue())
        }
        if (queue.isEmpty) {
          logger.info(s"Drained all $backlog buffers")
          if (closed) {
            logger.info(s"Stream previously closed, ending now")
            stream.end()
          }
        } else {
          logger.info(s"Drained ${backlog - queue.size} of $backlog buffers")
        }
    }
  )

  def closed(e: Any): Unit =
    if (queue.nonEmpty || !closed)
      closedPromise.failure(UnexpectedClose(str, queue))
    else
      closedPromise.success(())

  stream.on("close", closed)
  stream.on("finish", closed)

  stream.on(
    "error",
    {
      e: Any ⇒
        closedPromise.failure(UnexpectedError(str, queue, e.asInstanceOf[Throwable]))
    }
  )

  import scala.scalajs.js.typedarray._

  override def write(b: Int): Unit = write(Array[Byte](b.toByte))
  override def write(b: Array[Byte], off: Int, len: Int): Unit = write(b.slice(off, off + len))

  override def write(bytes: Array[Byte]): Unit = {
    if (closed)
      throw FileAlreadyClosedException(str)
    if (open)
      open = stream.write(bytes.toTypedArray.buffer).asInstanceOf[Boolean]
    else {
      queue.enqueue(bytes)
    }
  }

  override def flush(): Unit = {
    super.flush()
    stream.uncork()
    stream.cork()
  }

  override def close(): F[Unit] = {
    if (open)
      stream.end()
    else {
      logger.info(s"$str: stream draining; marking to close")
      closed = true
    }
    super.close()
    closedPromise.future
  }
}

object WritableStream {
  case class FileAlreadyClosedException(str: String)
    extends IOException(
      s"Attempting to write to a closed stream: $str"
    )
  case class UnexpectedClose(str: String, buffers: Seq[Array[Byte]])
    extends IOException(
      s"Received 'close' event with ${buffers.size} buffers queued (sizes: ${buffers.map(_.length).mkString(",")})"
    )

  case class UnexpectedError(str: String, buffers: Seq[Array[Byte]], cause: Throwable)
    extends IOException(
      s"Received 'error' event with ${buffers.size} buffers queued (sizes: ${buffers.map(_.length).mkString(",")})",
      cause
    )
}
