import scala.annotation.{tailrec, switch}
import scala.collection.immutable

/**
 * @author erusak.
 */
package object datastructures {
  sealed trait List[+A]

  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def foldRight[A,B](xs: List[A], z: B)(f: (A,B) => B): B = {
      xs match {
        case Nil => z            // here we cannot for example terminate the calculation if we encounter 0.0 because f
                                 // needs to evaluate its argument first (we will see how to use by name later on)
        case Cons(head, tail) => f(head, foldRight(tail, z)(f) )
      }
    }

    def foldLeft[A, B](xs: List[A], z: B)(f: (A,B) => B): B = {
      @tailrec
      def go(xs: List[A], acc: B): B = xs match {
        case Nil => acc
        case Cons(h, tail) => go(tail, f(h, acc))
      }
      
      go(xs,z)
      
    }
    
    def sum(xs: List[Int]): Int = xs match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }

    def sumFoldRight(xs: List[Int]) = {
      foldRight(xs, 0.0)(_ + _)
    }

    // Exercise 3_11
    def sumFoldLeft(xs: List[Int]) = {
      foldLeft(xs, 0.0)(_ + _)
    }

    def productFoldRight(xs: List[Double]) = {
      foldRight(xs, 1.0)(_ * _)
    }

    // Exercise 3_11
    def productFoldLeft(xs: List[Double]) = {
      foldLeft(xs, 1.0)(_ * _)
    }

    def product(xs: List[Double]): Double = xs match {
      case Nil => 1.0
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
        xs match {
          case Nil => Nil
          case list @ Cons(_, _) if step == 0 => list
          case Cons(_, tail) => doDrop(tail, step - 1)
        }
      }

      if(xs == Nil) throw new IllegalArgumentException
      else doDrop(xs, n)

    }

    //Exercise 3_5
    def dropWhile[A](xs: List[A], p: A => Boolean): List[A] = {
      xs match {
        case Nil => Nil
        case list @ Cons(head, _) if !p(head) => list
        case Cons(head, tail) => dropWhile(tail, p)
      }
    }

    //type inference will allow not to specify p type
    def dropWhileCurried[A](xs: List[A])(p: A => Boolean): List[A] = {
      dropWhile(xs, p)
    }

    // Exercise 3_6
    def init[A](xs: List[A]): List[A] = { //return all but last element
      xs match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(head, tail) => Cons(head, init(tail))
      }
    }

    // Exercise 3_6 - more efficient without copying and stackoverflow
    def initAnswer[A](xs: List[A]): List[A] = {
      import scala.collection.mutable.ListBuffer
      val buffer = new ListBuffer[A]

      @tailrec
      def go(xs: List[A]): List[A] = {
        xs match {
          case Nil => Nil
          case Cons(_, Nil) => List(buffer.toList: _*)
          case Cons(head, tail) => buffer += head; go(tail)
        }
      }

      go(xs)

    }

    // Exercise 3_9
    def lengthFoldRight[A](xs: List[A]): Int = {
      foldRight(xs, 0) {
        (_, acc) => acc + 1
      }
    }

    // Exercise 3_11
    def lengthFoldLeft[A](xs: List[A]): Int = {
      foldLeft(xs, 0) {
        (_, acc) => acc + 1
      }
    }

    // Exercise 3_12
    def reverseTailRec[A](xs: List[A]): List[A] = {
      import scala.collection.mutable.ListBuffer
      val buffer = new ListBuffer[A]

      @tailrec
      def go(xs: List[A]): List[A] = xs match {
        case Nil => List(buffer.toList: _*)
        case Cons(head, tail) => head +=: buffer; go(tail);
      }

      go(xs)

    }

    // Exercise 3_12 using fold
    def reverseWithFold[A](xs: List[A]): List[A] = {
      import scala.collection.mutable.ListBuffer
      val buffer = new ListBuffer[A]

      def func(x: A, buf: ListBuffer[A]) = x +=: buf

      foldLeft(xs, buffer)(func)

      List(buffer.toList: _*)

    }

    // Exercise 3_12 dumb me
    def reverse[A](xs: List[A]): List[A] = {

      foldLeft(xs, Nil: List[A])( (x, acc) => Cons(x, acc) )

    }

  }

}
