package datastructures

/**
 * @author erusak.
 */
object EntryPoint extends App {

  val cons = Cons(1, Cons(2, Cons(3, Nil)))
  val consD = List(1.0, 2.0, 3.0, 4.0)
  println(s"Sum of (1,2,3) is ${List.sum(cons)}")
  println(s"Product of (1,2,3,4) is ${List.product(consD)}")

  println(s"Sum of (1,2,3) with foldRight is ${List.sumFoldRight(cons)}")
  println(s"Product of (1,2,3,4) with foldRight is ${List.productFoldRight(consD)}")

  // Exercise 3_11
  println(s"Sum of (1,2,3) with foldLeft is ${List.sumFoldLeft(cons)}")
  println(s"Product of (1,2,3,4) with foldLeft is ${List.productFoldLeft(consD)}")
  println(s"Length of (1,2,3,4) with foldLeft is ${List.lengthFoldLeft(consD)}")


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


  //Exercise 3_12
  println(s"returning reversed for (1,2,3,4,5,7,6) using tailrec ${List.reverseTailRec(List(1,2,3,4,7,6))}")
  println(s"returning reversed for (1,2,3,4,5,7,6) using foldLeft ${List.reverse(List(1,2,3,4,7,6))}")
  println(s"returning reversed for (1,2,3,4,5,7,6) smart ${List.reverse(List(1,2,3,4,7,6))}")

  //Exercise 3_14
  println(s"returning append (8,7) to (1,2,3,4,7,6) using foldL ${List.appendFL(List(1,2,3,4,7,6), List(8,7))}")
  println(s"returning append (8,7) to (1,2,3,4,7,6) using foldR ${List.appendFR(List(1,2,3,4,7,6), List(8,7))}")

  //Exercise 3_15
  println(s"returning concat of ((1,2),(3),(4,7,6)) using foldL ${List.concat(List(List(1,2),List(3),List(4,7,6)))}")

  //Exercise 3_16
  println(s"incr list (1,0,2,3,4,5,10) by 1 ${List.incrList(List(1,0,2,3,4,5,10))}")
  //Exercise 3_16 with foldR
  println(s"incr list (1,0,2,3,4,5,10) by 1 with foldR ${List.incrListWithFoldR(List(1,0,2,3,4,5,10))}")

  //Exercise 3_17 convert doubles to strings
  println(s"convert doubles (1.1,0.0,2,3.7,4.1,5.1,10.111) to strings with foldR ${List.toStrings(List(1.1,0.0,2,3.7,4.1,5.1,10.111))}")

  //Exercise 3_19 remove odds using naive filter implementation
  println(s"remove odds from (1,0,2,3,4,5,10) ${List.filterNaive(List(1,0,2,3,4,5,10)) {(x: Int) => (x & 1) == 0 } }")
  //Exercise 3_19 remove odds using filter implementation with foldR
  println(s"remove odds from (1,0,2,3,4,5,10) ${List.filter(List(1,0,2,3,4,5,10)) {(x: Int) => (x & 1) == 0 } }")

  // Exercise 3_20 flat map with first naive implementation with foldRight and append
  println(s" (1,3,7,5,11) (i => List(i,i) === (1,1,3,3,7,7,5,5,11,11))  ${List.flatMap(List(1,3,7,5,11))( (i: Int) => List(i,i))}")

}
