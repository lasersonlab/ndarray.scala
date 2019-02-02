package org.lasersonlab.test

import lasersonlab.HasExecutionContext
import utest.TestSuite

import scala.concurrent.ExecutionContext

abstract class Suite(implicit val file: sourcecode.File)
  extends TestSuite
     with HasExecutionContext
     with Hooks
     with tmp.Paths
     with Resources {
  override implicit val ec = ExecutionContext.Implicits.global
}
