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

/**
  * The next part has a caveat. Always put "more complicated" type classes first,
  * there is a WONT_FIX bug in the compiler we need to work around.
  * */

implicit def zip[F[_], G[_, _], A, B](implicit F: Zip[F, G], a: F[A], b: F[B]): F[G[A, B]] =
  F.zip(a, b)

def zip3[F[_], G[_, _], A: F, B: F, C: F](implicit F: Zip[F, G]): F[G[A, G[B, C]]] =
  implicitly

def zip5[F[_], G[_, _], A: F, B: F, C: F, D: F, E: F](implicit F: Zip[F, G]): F[G[A, G[B, G[C, G[D, E]]]]] =
  implicitly

/**
  * Now, the business logic for Tuple2
  * */

zip5[List, Tuple2, Int, String, Double, Long, Char]
zip5[List, Tuple2, Int, String, Char, Long, Double]
zip5[List, Tuple2, String, Int, Double, Long, Char]

/**
  * for And
  * */

zip5[List, And, Int, String, Double, Long, Char]

/**
  * for Or
  * */

zip5[List, Or, Int, String, Double, Long, Char]

/**
  * The business logic need not change.
  * All we need to do is make sure all of our type class instances are in scope and
  * we can rearrange and redeclare at will.
  * Type Driven Development is the most declarative discipline I have found;
  * declare a bunch of types and instances, the compiler does the rest.
  * */