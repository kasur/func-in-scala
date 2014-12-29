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
}
