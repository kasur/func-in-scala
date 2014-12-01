/**
 * @author erusak.
 */
package object datastructures {
  sealed trait List[+A]

  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(xs: List[Int]): Int = xs match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }

    def product(xs: List[Double]): Double = xs match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(head, tail) => head * product(tail)
    }

    def apply[A](xs: A*): List[A] = {
      if(xs.isEmpty) Nil
      else {
        Cons(xs.head, apply(xs.tail: _*))
      }
    }

    // Exercise 3_2
    def tail[A](xs: List[A]): List[A] = {
      xs match {
        case Nil => throw new IllegalArgumentException
        case Cons(_, ys) => ys
      }
    }

  }
}
