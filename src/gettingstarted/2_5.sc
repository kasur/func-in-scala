// Implement the higher-order function that composes two functions
// def compose[A,B,C](f: A => B, g: B => C): A => C = ???

def compose[A,B,C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}

val g = (a: Int) => a + 5
val f = (b: Int) => b * 3

compose(f, g)(1) // (1 + 5) * 3 = 18