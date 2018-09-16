/**
  * Write a function which takes an id: Int and returns an Either depending upon which tree is evaluated if any.
  * Write a function which constructs TreeProcessor instances for Zip[List, Tuple2] instances
  */
trait Zip[F[_], G[_, _]]{
  def zip[A, B](a: F[A], b: F[B]): F[G[A, B]]
}
implicit def zipListTuple2: Zip[List, Tuple2] = new Zip[List, Tuple2]{
  override def zip[A, B](a: List[A], b: List[B]): List[(A, B)] = a.zip(b)
}
implicit def zip[F[_], G[_, _], A, B](implicit F: Zip[F, G], a: F[A], b: F[B]): F[G[A, B]] =
  F.zip(a, b)

implicit def a: List[Int] = List(1)
implicit def b: List[String] = List("2")
implicit def c: List[Double] = List(3.0)
implicit def d: List[Long] = List(4L)
implicit def e: List[Char] = List('5')

type Tree1 = List[((Char, Int), (String, Double))]
type Tree2 = List[((Int, (Char, String)), (Long, Double))]
val tree1 = implicitly[Tree1]
val tree2 = implicitly[Tree2]

trait TreeId[T]{
  def id: Int
}
trait TreeProcessor[Tree]{
  def process(in: Tree): Unit
}
trait Application[Id, Tree]{
  def process(id: Int): Either[String, Unit]
}
object Application{
  def process[Id, Tree](implicit treeId: TreeId[Id], tree: Tree, tProc: TreeProcessor[Tree]): Application[Id, Tree] =
    new Application[Id, Tree]{
      override def process(id: Int): Either[String, Unit] = {
        if(id == treeId.id){
          Right(tProc.process(tree))
        }else{
          Left("unmatched")
        }
      }
    }
}