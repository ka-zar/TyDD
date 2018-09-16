/**
  * We have a Tuple2 requirement which is a detail the Zip type class does not require for its definition.
  * Since the detail is unused, it can be forgotten.
  * We care about three types: Tuple2[A, B], And[A, B], Or[A, B].
  * They all take two proper type parameters. Let's try to replace this by a suitable variable.
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

/**
  * Just like we can abstract over types which have a single type parameter,
  * we can do the same with multiple type parameters!
  * And here are our instances.
  **/

implicit def zipListTuple2: Zip[List, Tuple2] = new Zip[List, Tuple2]{
  override def zip[A, B](a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
}

implicit def zipListAnd: Zip[List, And] = new Zip[List, And]{
  override def zip[A, B](a: List[A], b: List[B]): List[And[A, B]] = {
    val size = Math.min(a.size, b.size)
    (0 until size).toList.map{idx =>
      new And[A, B]{
        def left: A = a(idx)
        def right: B = b(idx)
      }
    }
  }
}

implicit def zipListOr: Zip[List, Or] = new Zip[List, Or]{
  override def zip[A, B](a: List[A], b: List[B]): List[Or[A, B]] = {
    val size = Math.max(a.size, b.size)
    (0 until size).toList.map{idx =>
      new Or[A, B]{
        def left: Option[A] = if(a.size < idx) Some(a(idx)) else None
        def right: Option[B] = if(b.size < idx) Some(b(idx)) else None
      }
    }
  }
}

implicit def a: List[Int] = List(1)
implicit def b: List[String] = List("2")
implicit def c: List[Double] = List(3.0)
implicit def d: List[Long] = List(4L)
implicit def e: List[Char] = List('5')

