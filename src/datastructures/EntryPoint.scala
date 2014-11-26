package datastructures

/**
 * @author erusak.
 */
object EntryPoint extends App {

  val cons = Cons(1, Cons(2, Cons(3, Nil)))
  val consD = List(1.0, 2.0, 3.0)
  println(List.sum(cons))
  println(List.product(consD))

  val result = List(1,2,3,4,5,6,7) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  println(s"result is $result")

}
