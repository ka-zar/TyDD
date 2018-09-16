
/***
  *  Using only values and functions (no classes, no traits)
  *  Write a function which can zip 3 Lists.
  */

def zipS[A, B, C](a: List[A], b: List[B], c: List[C]): List[(A, (B, C))] =
  a.zip(b.zip(c))

def zipS[A, B, C](a: Vector[A], b: Vector[B], c: Vector[C]): Vector[(A, (B, C))] =
  a.zip(b.zip(c))

/***
  * These functions are identical!
  * Let's abstract the container
  */

def zipK[F[_], A, B](a: F[A], b: F[B]): F[(A, B)] =
  ???

def zip3K[F[_], A, B, C](a: F[A], b: F[B], c: F[C]): F[(A, (B, C))] =
  zipK(a, zipK(b, c))

/***
  * We have a problem. Now we don't know what functions can be used.
  * We no longer have a List or Vector we have instead an F.
  * Let's pass a function in with the values.
  */

def zipF[F[_], A, B](a: F[A], b: F[B], f: (F[A], F[B]) => F[(A, B)]): F[(A, B)] =
  f(a, b)
def zip3F[F[_], A, B, C](a: F[A], b: F[B], c: F[C], f1: (F[B], F[C]) => F[(B, C)], f2: (F[A], F[(B, C)]) => F[(A, (B, C))]): F[(A, (B, C))] =
  zipF(a, zipF(b, c, f1), f2)

/***
  * Now the version with 3 has become super cumbersome.
  * But they are basically the same thing, surely we can abstract this out.
  */


trait Zip[F[_]]{
  def zip[A, B](a: F[A], b: F[B]): F[(A, B)]
}

/***
  * It is common practice to use a trait to represent a function which has been abstracted.
  * This is referred to as a type class encoding.
  */

def zip[F[_], A, B](a: F[A], b: F[B], F: Zip[F]): F[(A, B)] =
  F.zip(a, b)

def zip3[F[_], A, B, C](a: F[A], b: F[B], c: F[C], F: Zip[F]): F[(A, (B, C))] =
  F.zip(a, F.zip(b, c))


/***
  * When we abstract a value, we get a type parameter; this helps us in a sense bind a value to a type.
  * The same thing goes for functions except we need type classes to do so.
  * Type classes sit at a higher level of abstraction; they take type parameters.
  * Note: This is not the typical definition of a type class.
  * This definition is just super convenient for TyDD.
  */