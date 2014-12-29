package errorhandling

/**
 *
 *
 *
 * @author erusak.
 */
object EntryPoint extends App {

  // Exercise 4_2
  println(s"mean of (1,2,3,4,5,6,7,8) is ${mean(Seq[Double](1,2,3,4,5,6,7,8))}")
  println(s"variance  of (1,2,3,4,5,6,7,8) is ${variance(Seq[Double](1,2,3,4,5,6,7,8))}")
  println(s"mean of () is ${mean(Seq[Double]())}")
  println(s"variance of () is ${variance(Seq[Double]())}")
}
