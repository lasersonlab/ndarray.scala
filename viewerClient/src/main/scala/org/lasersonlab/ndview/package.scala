package org.lasersonlab

import org.lasersonlab.ndview.view.TreeState

package object ndview {
  type Route   = Vector[String]
   def Route() = Vector()

  type ClosedFolders   = TreeState[Boolean]
   def ClosedFolders() = TreeState[Boolean]()
}
