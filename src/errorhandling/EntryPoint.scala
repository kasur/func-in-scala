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

  // Exercise 4_3
  println(s""" Convert int strings ("1", "3") to Double ${parseQuote("1", "3")}""")
  println(s""" Convert int strings ("1fg", "3") to Double ${parseQuote("1fg", "3")}""")
  println(s""" Convert int strings ("1", "3dddf") to Double ${parseQuote("1", "3dddf")}""")
  println(s""" Convert int strings ("wre", "3sdf") to Double ${parseQuote("wre", "3sdf")}""")

  // Exercise 4_4
  println(s"From List(Some(1), Some(2), Some(3)) getting Some(List(1,2,3)) ${Option.sequence_rec(List(Some(1), Some(2), Some(3)))}")
  println(s"From List(Some(1), Some(2), Some(3)) getting Some(List(1,2,3)) ${Option.sequence_fold(List(Some(1), Some(2), Some(3)))}")
  println(s"From List(Some(1), None, Some(3)) getting None) ${Option.sequence_rec(List(Some(1), None, Some(3)))}")
  println(s"From List(Some(1), None, Some(3)) getting None) ${Option.sequence_fold(List(Some(1), None, Some(3)))}")
  println(s"From List(Some(1), Some(2), None) getting None) ${Option.sequence_rec(List(Some(1), Some(2), None))}")
  println(s"From List(Some(1), Some(2), None) getting None) ${Option.sequence_fold(List(Some(1), Some(2), None))}")
  println(s"From List(None, None, None) getting None) ${Option.sequence_rec(List(None, None, None))}")
  println(s"From List(None, None, None) getting None) ${Option.sequence_fold(List(None, None, None))}")
}
