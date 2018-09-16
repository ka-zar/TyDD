/**
  * Exercises
  * Define The Non-Empty Binary Tree
  * */

/**
  * We can take Tuple2 as G but, what for F?
  * We need a type which given an A ensures an A for any type A.
  * In other words we need an Identity at the type level.
  * */

trait Zip[F[_], G[_, _]]{
  def zip[A, B](a: F[A], b: F[B]): F[G[A, B]]
}


trait And[A, B]{
  def left: A
  def right: B
}

trait Or[A, B]{
  def left: Option[A]
  def right: Option[B]
}

type Identity[A] = A

/**
  * Yes, this exploits an unsoundness in the type system but,
  * we can do anything we want as long as we are careful
  * */

implicit def nonEmptyBinaryTree: Zip[Identity, Tuple2] = new Zip[Identity, Tuple2]{
  override def zip[A, B](a: Identity[A], b: Identity[B]): Identity[(A, B)] = (a, b)
}

implicit def zip[F[_], G[_, _], A, B](implicit F: Zip[F, G], a: F[A], b: F[B]): F[G[A, B]] =
  F.zip(a, b)

implicit def aAsId: Identity[Int] = 1
implicit def bAsId: Identity[String] = "2"
implicit def cAsId: Identity[Double] = 3.0
implicit def dAsId: Identity[Long] = 4L
implicit def eAsId: Identity[Char] = '5'

implicitly[Identity[((Int, Long), (Char, (String, Double)))]]

/**
  * Note: When we change from Identity to List we get a List of non empty binary trees.
  * In practice,
  * it is a much better idea to have a singleton List than an Identity for soundness in the type system.
  */

implicit def aAsList: List[Int] = List(1)
implicit def bAsList: List[String] = List("2")
implicit def cAsList: List[Double] = List(3.0)
implicit def dAsList: List[Long] = List(4L)
implicit def eAsList: List[Char] = List('5')