import scala.annotation.switch

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

    //Exercise 3_3
    def setHead[A](xs: List[A], head: A): List[A] = {
      xs match {
        case Nil => throw new IllegalArgumentException
        case Cons(_, tail) => Cons(head, tail)
      }
    }

    // Exercise 3_4
    def drop[A](xs: List[A], n: Int): List[A] = {

      def doDrop(xs: List[A], step: Int): List[A] = {
        (xs: @switch) match {
          case Nil => Nil
          case list @ Cons(_, _) if step == 0 => list
          case Cons(_, tail) => doDrop(tail, step - 1)
        }
      }

      if(xs == Nil) throw new IllegalArgumentException
      else doDrop(xs, n)

    }

  }
}
