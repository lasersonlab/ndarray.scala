package org.lasersonlab.test

import lasersonlab.HasExecutionContext
import utest.TestSuite

abstract class Suite(implicit val file: sourcecode.File)
  extends TestSuite
     with HasExecutionContext
     with Hooks
     with tmp.Paths
     with Resources {
  override implicit val ec = lasersonlab.threads.pool.default
}
