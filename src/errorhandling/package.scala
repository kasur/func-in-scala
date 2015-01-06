/**
 * @author erusak.
 */
package object errorhandling {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this map(f) getOrElse None

    def orElse[B >: A](obj: => Option[B]): Option[B] = this map(Some(_)) getOrElse obj

    def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)

  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // companion object for Option
  object Option {
    // Exercise 4_4 with recursion
    def sequence_rec[A](list: List[Option[A]]): Option[List[A]] = list match {
      case Nil => Some(Nil)
      case headO :: tail => headO flatMap { head => sequence_rec(tail) map (l => head :: l)}
    }
    // Exercise 4_4 with foldLeft and map2
    def sequence_fold[A](list: List[Option[A]]): Option[List[A]] = {
      list.foldRight[Option[List[A]]](Some(Nil): Option[List[A]]) {
        (z,optionA) => map2(z, optionA)(_ :: _)
      }
    }

    // Exercise 4_5
    def traverse[A,B](list: List[A])(f: A => Option[B]): Option[List[B]] = {
      list.foldRight[Option[List[B]]](Some(Nil): Option[List[B]]) {
        (a,z) => map2(f(a), z)(_ :: _)
      }
    }
    // Exercise 4_5
    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(x => x)
  }


  // Exercise 4_2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap {
      //reusing mean hear
      m => mean( xs.map( x => math.pow( x - m, 2)) )
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None else Some(xs.sum / xs.length)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 0.123d

  def parseQuote(ageS: String, ticketsS: String): Option[Double] = {
    val ageO = Try { ageS.toInt }
    val ticketsO = Try { ticketsS.toInt }
    map2(ageO, ticketsO)(insuranceRateQuote)
  }

  def Try[A](arg: => A): Option[A] = {
    try Some(arg)
    catch { case ex: Exception => None }
  }

  // Exercise 4_3
  def map2[A,B,C](option1: Option[A], option2: Option[B])(f: (A,B) => C): Option[C] = (option1, option2) match {
    case (Some(v1), Some(v2)) => Some(f(v1,v2))
    case _ => None
  }

}
