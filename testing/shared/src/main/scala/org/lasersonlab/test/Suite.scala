package org.lasersonlab.test

import utest.TestSuite

import scala.concurrent.ExecutionContext

abstract class Suite
  extends TestSuite
     with HasExecutionContext
     with Hooks
     with tmp.Paths
     with Resources {
  override implicit val ec = ExecutionContext.Implicits.global
}
