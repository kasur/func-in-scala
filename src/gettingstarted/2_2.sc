import scala.annotation.tailrec

def isSorted[A](xs: Array[A], isOrdered: (A, A) => Boolean): Boolean = {

  @tailrec
  def _step(n: Int): Boolean = {
    if(n - 1 < 0) true
    else {
      if( !isOrdered(xs(n - 1 ), xs(n)) ) false
      else _step(n - 1)
    }
  }

  val len = xs.length
  if(len <= 1) true
  else {
    _step(len - 1)
  }

}

val func = (_: Int) <= (_: Int)

isSorted(Array(1,2,3,4,5,6,7), func )
isSorted(Array(1,2,3,4,2,6,7), func )


