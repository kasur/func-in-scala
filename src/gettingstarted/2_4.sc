// Implement uncurry, which reverses the transformation of curry.
// Note that since => associate to the right, A => (B => C) can be rewritten
// as A => B => C
// def uncurry(f: A => B => C): (A, B) => C

def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a,b) => f(a)(b)
}

val curry = (a: Int) => (b: Int) => a + b

curry(2)(5)
uncurry(curry)(2,5)
