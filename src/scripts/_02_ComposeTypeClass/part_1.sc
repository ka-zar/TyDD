

trait Zip[F[_]]{
  def zip[A, B](a: F[A], b: F[B]): F[(A, B)]
}

def zip[F[_], A, B](a: F[A], b: F[B], F: Zip[F]): F[(A, B)] =
  F.zip(a, b)

/***
  * Here is a Zip instance for List
  */

def zipList: Zip[List] = new Zip[List]{
  override def zip[A, B](a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
}

zip(List('a', 'b', 'c'), List(1, 2, 3), zipList)

/***
  * It is clear a Zip[List] is required at the call site,
  * it seems unreasonable to pass it each time we need to call the function especially when building larger zip calls
  */

object CumberSome {
  def zip3[F[_], A, B, C](a: F[A], b: F[B], c: F[C], F: Zip[F]): F[(A, (B, C))] =
    zip(a, zip(b, c, F), F)
  def zip4[F[_], A, B, C, D](a: F[A], b: F[B], c: F[C], d: F[D], F: Zip[F]): F[(A, (B, (C, D)))] =
    zip(a, zip(b, zip(c, d, F), F), F)
  def zip5[F[_], A, B, C, D, E](a: F[A], b: F[B], c: F[C], d: F[D], e: F[E], F: Zip[F]): F[(A, (B, (C, (D, E))))] =
    zip(a, zip(b, zip(c, zip(d, e, F), F), F), F)
}

/***
  * This is more cumbersome than it seems necessary.
  * Through the implicit keyword, Scala provides a mechanism for threading type class instances through call stacks
  */

object ThreadedZip {
  implicit def zip[F[_], A, B](implicit a: F[A], b: F[B], F: Zip[F]): F[(A, B)] =
    F.zip(a, b)
  implicit def zipList: Zip[List] = new Zip[List]{
    override def zip[A, B](a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
  }

  def zip3[F[_], A, B, C](implicit a: F[A], b: F[B], c: F[C], F: Zip[F]): F[(A, (B, C))] =
    implicitly
  def zip4[F[_], A, B, C, D](implicit a: F[A], b: F[B], c: F[C], d: F[D], F: Zip[F]): F[(A, (B, (C, D)))] =
    implicitly
  def zip5[F[_], A, B, C, D, E](implicit a: F[A], b: F[B], c: F[C], d: F[D], e: F[E], F: Zip[F]): F[(A, (B, (C, (D, E))))] =
    implicitly
}

object Shorter {
  implicit def zip[F[_], A, B](implicit a: F[A], b: F[B], F: Zip[F]): F[(A, B)] =
    F.zip(a, b)
  implicit def zipList: Zip[List] = new Zip[List]{
    override def zip[A, B](a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
  }

  def zip3[F[_]: Zip, A: F, B: F, C: F]: F[(A, (B, C))] =
    implicitly
  def zip4[F[_]: Zip, A: F, B: F, C: F, D: F]: F[(A, (B, (C, D)))] =
    implicitly
  def zip5[F[_]: Zip, A: F, B: F, C: F, D: F, E: F]: F[(A, (B, (C, (D, E))))] =
    implicitly

  /***
    * If you ask how zip3 looks in .class file
    * def zip3[F[_] >: [_]Nothing <: '[_]Any, A, B, C](implicit evidence$1: A'$A239.this.Zip[F],
    *                                                 evidence$2: F[A],
    *                                                 evidence$3: F[B],
    *                                                 evidence$4: F[C]): F[(A, (B, C))] =
    *                                                   A$A239.this.
    *                                                   implicitly[F[(A, (B, C))]](
    *                                                     A$A239.this.zip[F, A, (B, C)](
    *                                                       evidence$2,
    *                                                       A$A239.this.zip[F, B, C](
    *                                                         evidence$3,
    *                                                         evidence$4,
    *                                                         evidence$1),
    *                                                       evidence$1));
    */


  /**
    * Here we can see similar syntax moves can be made with our type class, Zip, instance and our List instances.
    * List is a type class (the Free Monoid).
    */
}