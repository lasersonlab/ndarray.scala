package org.lasersonlab.uri.io

import lasersonlab.future
import lasersonlab.future.F

import scala.concurrent.ExecutionContext

/**
 * [[java.io.OutputStream]] wrapper with coarse-grained asynchrony: [[OutputStream(.close]] returns a
 * [[scala.concurrent.Future]] which completes when the stream is closed (having successfully written all requested
 * data; otherwise an error is thrown)
 */
//abstract class OutputStream(implicit ec: ExecutionContext) {
//  @inline def write(b: Int): Unit = write(Array(b.toByte))
//  @inline def write(b: Array[Byte]): Unit = write(b, 0, b.length)
//  def write(b: Array[Byte], off: Int, len: Int): Unit
//  def flush(): Unit = {}
//  def close(): F[Unit] = F { (()) }
//}
//
//object OutputStream {
//  def apply(os: java.io.OutputStream)(implicit ec: ExecutionContext): OutputStream =
//    new OutputStream {
//      @inline override def write(b: Array[Byte], off: Int, len: Int): Unit = os.write(b, off, len)
//      @inline override def write(b: Int): Unit = os.write(b)
//      @inline override def write(b: Array[Byte]): Unit = os.write(b)
//      @inline override def flush(): Unit = os.flush()
//      @inline override def close(): future.F[Unit] = F { os.close() }
//    }
//}
