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

implicit def aAsList: List[Int] = List(1)
implicit def bAsList: List[String] = List("2")
implicit def cAsList: List[Double] = List(3.0)
implicit def dAsList: List[Long] = List(4L)
implicit def eAsList: List[Char] = List('5')

/**
  * This is all well and good but eventually we have to accept user input and perform specific actions
  * based upon that input.
  * How can we model control structures at the type level?
  * Well, let's start with doing it at the value level and abstract from there.
  */

implicit def a: List[((Char, Int), (String, Double))] = List((('2', 2), ("3", 4.0)))
implicit def b: List[((Int, (Char, String)), (Long, Double))] = List(((1, ('2', "4")), (5L, 8.0)))
val tree1 = implicitly[List[((Char, Int), (String, Double))]]
val tree2 = implicitly[List[((Int, (Char, String)), (Long, Double))]]

def process[F[_], G[_, _], A, B](in: F[G[A, B]]): Unit ={
  // Do whatever we want with the tree
  // Hopefully something very fancy and incredibly heroic!!!
  ()
}
def application(id: Int): Either[String, Unit] =
  if(1 == id){
    Right(process(tree1))
  }else if(2 == id){
    Right(process(tree2))
  }else{
    Left(s"invalid tree id $id")
  }

/**
  * So, what we need is a way to bind an id to a tree and then a way to bind that id to a type.
  * We lift everything into the type system
  */

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