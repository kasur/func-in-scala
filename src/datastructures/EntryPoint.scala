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

  // Exercise 3_2
  val tail_1 = List.tail(cons)
  println(s"Tail is $tail_1")

  // Exercise 3_3
  val headed = List.setHead(List(1,2,3,4), 7) //should be 7,2,3,4
  println(headed == List(7,2,3,4))

  // Exercise 3_4
  val dropped = List.drop( List(1,2,3,4,5), 3)
  println(dropped)

  // Exercise 3_5
  println(s"dropping (1,2,3,4,5) with _ > 0  => ${List.dropWhile(List(1,2,3,4,5), (_: Int) > 0)}")
  println(s"dropping (1,2,3,4,5) with _ <= 3  => ${List.dropWhile(List(1,2,3,4,5), (_: Int) <= 3 )}")
  println(s"dropping (1,2,3,4,5) with _ < 0  => ${List.dropWhile(List(1,2,3,4,5), (_: Int) < 0 )}")
  println(s"dropping (1,2,3,4,5) with _ <= 2  => ${List.dropWhileCurried(List(1,2,3,4,5))(_ <= 2)}")

  // Exercise 3_6
  println(s"returning (1,2,3,4,5) with the last dropped out ${List.init(List(1,2,3,4,5))}")
  println(s"returning (1) with the last dropped out ${List.init(List(1))}")

  println(s"returning (1,2,3,4,5) with the last dropped out ${List.initAnswer(List(1,2,3,4,5))}")
  println(s"returning (1) with the last dropped out ${List.initAnswer(List(1))}")

}
