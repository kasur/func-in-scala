// implement curring
//def curry[A,B,C](f: (A, B) => C): A => B => C

def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  //That is my own solution
  /*def part(a: A): B => C = {
    f(a, _)
  }
  part*/

  a => b => f(a,b)

}

val testF = (a: Int, b: Int) => a + b
val curried = curry(testF)

curried(1)(4)


