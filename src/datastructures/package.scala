import scala.annotation.tailrec

/**
 * @author erusak.
 */
package object datastructures {
  // custom list implementation
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

    // Exercise 3_14 : append with either foldL
    def appendFL[A](xs: List[A], xz: List[A]): List[A] = {
      foldLeft(reverse(xs), xz)(Cons(_,_))
    }

    // Exercise 3_14 : append with either foldR
    def appendFR[A](xs: List[A], xz: List[A]): List[A] = {
      foldRight(xs, xz)(Cons(_,_))
    }

    // Exercise 3_15: concat list of lists into a single list
    def concat[A](xs: List[List[A]]): List[A] = {
      foldRight(xs, Nil: List[A]) ( appendFR )
    }

    // Exercise 3_16
    def incrList(xs: List[Int]): List[Int] = xs match {
      case Nil => Nil
      case Cons(head, tail) => Cons(head + 1, incrList(tail))
    }

    // Exercise 3_16 with foldR
    def incrListWithFoldR(xs: List[Int]): List[Int] = {
      foldRight(xs, Nil: List[Int])( (head, tail) => Cons(head + 1, tail) )
    }

    // Exercise 3_17 convert doubles to strings
    def toStrings(xs: List[Double]): List[String] = {
      foldRight(xs, Nil: List[String]) { (h,t) => Cons(h.toString, t)}
    }

    // Exercise 3_18 map implementation
    def map[A,B](xs: List[A])(f: A => B): List[B] = foldRight(xs, Nil: List[B]) { (h,t) => Cons(f(h), t) }

    // Exercise 3_19 filter naive implementation
    def filterNaive[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
      case Nil => Nil
      case Cons(h,t) if p(h) => Cons(h, filterNaive(t)(p))
      case Cons(_, t) => filterNaive(t)(p)
    }

    // Exercise 3_19 filter implementation with foldR
    def filter[A](xs: List[A])(p: A => Boolean): List[A] = {
      foldRight(xs, Nil: List[A]) {
        (x, ys) => if (p(x)) Cons(x, ys) else ys
      }
    }

    // Exercise 3_20 implement flatMap
    def flatMap[A,B](xs: List[A])(f: A => List[B]): List[B] = {
      foldRight(xs, Nil: List[B]) {
        (as: A, t: List[B]) => appendFR(f(as), t)
      }
    }

    // Exercise 3_20 implement flatMap using map and concat
    def flatMap_1[A,B](xs: List[A])(f: A => List[B]): List[B] = {
      concat(map(xs)(f))
    }

    // Exercise 3_21 use flatMap to implement filter
    def filter_flatMap[A](xs: List[A])(p: A => Boolean): List[A] = {
      flatMap_1(xs){ a => if(p(a)) List(a) else Nil }
    }

    // Exercise 3_22 zip with addition for Int type
    def zipInts(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1 + h2, zipInts(t1, t2))
    }

    // Exercise 3_23 generalize zip for any operation
    def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C ): List[C] = (xs, ys) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }

    private def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2,t2))  if h1 == h2 => startsWith(t1,t2)
      case _ => false
    }

    // Exercise 3_24 initial attempt to implement hasSubsequence functionally
    @tailrec
    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
      case Nil => false
      case Cons(h,t) if startsWith(l, sub) => true
      case Cons(h,t) => hasSubsequence(t, sub)
    }


  }

}
