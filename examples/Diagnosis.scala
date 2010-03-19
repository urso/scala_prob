
/*
 * Problem: 
 * Given a positive or negative test for a specific illness we wan't to know the
 * probability for being ill or healthy.
 *
 * Suppose the random variables I and T are given with I = {Ill, Healthy}
 * being the health status and T = {Negative, Positive} the test result.
 *
 * It is known that the probability of being ill is 1 in a 1000,
 * thus:
 * P(I = Ill) = 0.001 and P(I = Healthy) = 0.999
 *
 * Furthermore we do know that the test has an accuracy of 99%, thus
 * P(T = Positive | I = Ill ) = 0.99
 * P(T = Negative | I = Ill ) = 0.01
 * P(T = Positive | I = Healthy ) = 0.01
 * P(T = Negative | I = Healthy ) = 0.99
 *
 * Task:
 * compute the probability of being ill, given a test was positive.
 * Using bayes rule:
 *
 * P(T, I) = P(T|I) * P(I) = P(I|T) * P(T)
 *
 * =>
 *
 *           P(T |I) * P(I)
 * P(I|T) = ---------------- = < P(T|I) * P(I) >
 *                P(T)
 *
 */
object Diagnosis {
  import probability.EmbeddedProbability._

  sealed abstract class Status
  case class Ill() extends Status
  case class Healthy() extends Status

  sealed abstract class Test
  case class Negative() extends Test
  case class Positive() extends Test

  val PFalseNegative = 0.01 // constant for P( T | I = Ill)
  val PFalsePositive = 0.01 // constant for P( T | I = Healthy)

  // define P(I)
  def pDisease = flip(0.001, Ill(), Healthy())

  // define P(T|I)
  def pTest(d:Status) = flip(if (d == Ill()) PFalseNegative else PFalsePositive,
                             Negative(), Positive())

  // compute P(I|T=Positive):
  def run = {
    println("P(I|T=Positive) = ")
    println(normalizedProb[Status] {
          val i = pDisease
          if ( Positive() == pTest(i) ) Some(i) else None
        })
  }

  def main(args:Array[String]) = run

}

