
/*
 * Alarm example from "Artificial Intelligence - A Modern Approach" by Russel
 * and Norvig Page 493 cc.
 * 
 * Suppose you have a new fairly reliable burglar alarm at home but occasionally
 * it responds to minor earthquakes. You also have two neighbors John and Mary,
 * who have promised to call you at work when they hear the alarm. John always
 * calls when he hears the alarm, but sometimes confuses the telephone ringing
 * with the alarm and calls then, too. Mary, on the other hand, is too much in
 * loud music and sometimes misses the alarm altogether.
 * 
 * So the bayesian network will be:
 * 
 *           B         E
 *            \       /
 *            _\|   |/_
 *                A
 *             /    \
 *           |/_    _\|
 *          J          M
 * 
 *  with probabilities:
 *  P(B) = 0.001
 *  P(E) = 0.002
 * 
 *  P(A| B=true, E=true)   = 0.95
 *  P(A| B=true, E=false)  = 0.94
 *  P(A| B=false, E=true)  = 0.29
 *  P(A| B=false, E=false) = 0.001
 * 
 *  P(J| A=true)  = 0.9
 *  P(J| A=false) = 0.05
 * 
 *  P(M| A=true)  = 0.7 
 *  P(M| A=false) = 0.01
 * 
 *  where B = burglar, E = earthquake, A = alarm, J = John calls and 
 *  M = Mary calls
 * 
 * ----------------------------------------------------------------------------
 */
object Alarm {
  import probability.EmbeddedProbability._

  //first we want to encode the events
  case class Burglary(s:Boolean)
  case class Earthquake(s:Boolean)
  case class Alarm(s:Boolean)
  case class John(s:Boolean)
  case class Mary(s:Boolean)

  case class State(b:Burglary, e:Earthquake, a:Alarm, j:John, m:Mary)

  // and a smart distribution constructor for the events
  def mkProb[A](p:Double, mk:Boolean => A) = flip(p, mk(true), mk(false))

  // let's encode the network's probabilities by translating the network's 
  // probabilities into distribution functions:
  def burglary   = mkProb(0.001, Burglary)
  def earthquake = mkProb(0.002, Earthquake)

  def alarm(b:Burglary, e:Earthquake) =
    mkProb( (b.s, e.s) match {
      case (true, true) => 0.95
      case (true, false) => 0.94
      case (false, true) => 0.29
      case (false, false) => 0.001
    }, Alarm)

  def john(a:Alarm) = mkProb( if(a.s) 0.9 else 0.05, John )
  def mary(a:Alarm) = mkProb( if(a.s) 0.7 else 0.01, Mary )

  //compute the joint probability:
  val p = prob[State] {
    val b = burglary
    val e = earthquake
    val a = alarm(b,e)
    State(b, e, a, john(a), mary(a))
  }

  def main(args:Array[String]) = run

  // let's try different methods of the probability
  // a burglar happens if john and mary call:
  // P(B|John=true, Mary=true)
  def run = {
      println("P(B|John=true, Mary=true) : ")

      println("using joint probability: " + 
          p.filter{ s => s.j.s == true && s.m.s == true }.map { s => s.b })

      // this direct method will automatically marginalize, filter and
      // normalize the probability + the number of needed multiplications 
      // and space usage is much reduce in comparison to the joint probability
      // solution.
      println("do bayesian inference directly: " +
          normalizedProb[Burglary] { 
            val b = burglary
            val e = earthquake
            val a = alarm(b,e)

            // do conditional filtering directly
            if (john(a).s && mary(a).s) Some(b) else None
          })
  }
}

