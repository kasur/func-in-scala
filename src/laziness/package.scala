import scala.annotation.tailrec

/**
 * @author erusak.
 */
package object laziness {

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

    // Exercise 5_1 - prone to stack overflow issue
    def toListSO: List[A] = this match {
      case Cons(h,t) => h() :: t().toListSO
      case Empty => List.empty
    }

    // Exercise 5_1 - with the accumulator (this is the most functional way i guess)
    def toList: List[A] = {
      @tailrec
      def _toList(stream: Stream[A], result: List[A]): List[A] = stream match{
        case Empty => result
        case Cons(h,t) => _toList(t(), h() :: result)
      }
      _toList(this, List.empty).reverse
    }

    // Exercise 5_2
    def take(n: Int): Stream[A] = this match {
      case Cons(h,t) if n > 1 => Stream.cons(h(), take(n - 1))
      case Cons(h,t) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }

    // Exercise 5_3
    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(h,t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    // Exercise 5_3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h,t) if p(h()) => Stream.cons(h(), t() takeWhile p)
      case _ => Stream.empty
    }


  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {

    def empty[A]: Stream[A] = Empty

    def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
      lazy val h = head
      lazy val t = tail
      Cons(() => h, () => t)
    }

    def apply[A](xs: A*): Stream[A] = {
      if(xs.isEmpty) empty else cons(xs.head, apply(xs.tail: _*))
    }

  }

}
