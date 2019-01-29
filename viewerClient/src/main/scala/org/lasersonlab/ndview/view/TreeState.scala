package org.lasersonlab.ndview.view

case class TreeState[T](
  value: Option[T] = None,
  children: Map[String, TreeState[T]] = Map.empty[String, TreeState[T]],
) {
  def /(child: String): Option[TreeState[T]] = children.get(child)
  def /(path: Vector[String]): Option[TreeState[T]] =
    path match {
      case Vector() ⇒ Some(this)
      case head +: tail ⇒
        for {
          child ← children.get(head)
          leaf ← child / tail
        } yield
          leaf
    }

  def set(path: Vector[String], value: T): TreeState[T] = set(path, Some(value))
  def set(path: Vector[String], value: Option[T]): TreeState[T] =
    path match {
      case Vector() ⇒ copy(value = value)
      case head +: tail ⇒
        copy(
          children =
            children + ((
              head,
              children.getOrElse(
                head,
                TreeState()
              )
              .set(tail, value)
            ))
        )
    }

  def clear(path: Vector[String]) = set(path, None)
}
object TreeState {
  implicit def unwrap[T](treeState: TreeState[T]) = treeState.children
}
