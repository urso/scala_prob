

object Test1 {
  import probability.EmbeddedProbability._

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

  def test3 = prob[(Int,Int)] {
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

  def test6 = collecting[Int](loopK(100000)) {
    val d1 = die
    val d2 = die
    d1 + d2
  }

  //run test for 30 seconds
  def test7 = collecting[Int](loopMaxMs(1000 * 30)) {
    val d1 = die
    val d2 = die
    d1 + d2
  }

  def test8 = pickValue( runProbabilistic[(Int, Int)] {
        (die, die)
      })

  def test9 = pickValue( runProbabilistic[Int] {
    die + die
  })

  def test10 = prob[String] {
    if (coin) "head" else "tail"
  }

  def test11 = prob[Option[String]] {
    (coin, coin) match {
      case (true, true)   => Some ("heads")
      case (false, false) => Some ("tails")
      case (_, _)              => None
    }
  }

  def test12 = normalizedProb[String] {
    (coin, coin) match {
      case (true, true)   => Some ("heads")
      case (false, false) => Some ("tails")
      case (_, _)              => None
    }
  }

}

