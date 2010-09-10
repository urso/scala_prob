

object Test1 {
  import probability.probdsl._

  def test = prob[Boolean] {
    flip(0.5)
  }

  def test2 = prob[Boolean] {
    val x = flip(0.5)
    val y = flip(0.5)
    x | y
  }

  def die = uniform[Int](Array(1,2,3,4,5,6))
  def coin = flip(0.5)

  def test3 = normalizedProb[(Int,Int)] {
    val d1 = die
    val d2 = die
    (d1, d2)
  }

  def test4 = prob[Int] {
    val d1 = die
    val d2 = die
    d1 + d2
  }

  def test5 = prob[Int] {
    val d = dist(test3)
    d._1 + d._2
  }

  def test6 = collect[Int](loopK(100000)) {
    val d1 = die
    val d2 = die
    d1 + d2
  }

  /*//run test for 30 seconds*/
  def test7 = collect[Int](loopMaxMs(1000 * 30)) {
    val d1 = die
    val d2 = die
    d1 + d2
  }

  def test8 = normalizedProb[Int] {
    die + die
  }.pick

  def test9 = normalizedProb[String] {
    if (coin) "head" else "tail"
  }

  // truth table
  def test10 = prob[String] {
    (coin, coin) match {
      case (true, true)   => single("heads")
      case (false, false) => single("tails")
      case (_, _)         => stop
    }
  }

  // truth table with normalization
  def test11 = normalizedProb[String] {
    (coin, coin) match {
      case (true, true)   => single("heads")
      case (false, false) => single("tails")
      case (_, _)         => stop
    }
  }
}

