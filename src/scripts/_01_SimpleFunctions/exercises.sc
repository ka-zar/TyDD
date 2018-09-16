/***
  * Exercises
  * Write a type class which is both A and B.
  * Write a type class which is only A or only B.
  */

trait And[A, B]{
  def left: A
  def right: B
}

trait Or[A, B]{
  def left: Option[A]
  def right: Option[B]
}