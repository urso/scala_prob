
object Alarm {
  import probability.EmbeddedProbability._

  case class Bulgary(s:Boolean)
  case class Earthquake(s:Boolean)
  case class Alarm(s:Boolean)
  case class John(s:Boolean)
  case class Mary(s:Boolean)

  case class State(b:Bulgary, e:Earthquake, a:Alarm, j:John, m:Mary)

  def mkProb[A](p:Double, mk:Boolean => A) = flip(p, mk(true), mk(false))

  val p = prob[State] {
    var b = mkProb(0.001, Bulgary)
    var e = mkProb(0.002, Earthquake)
    var a = mkProb( (b.s, e.s) match {
                      case (true, true) => 0.95
                      case (true, false) => 0.94
                      case (false, true) => 0.29
                      case (false, false) => 0.001
                    }, Alarm)
    var j = mkProb( if(a.s) 0.9 else 0.05, John)
    var m = mkProb( if(a.s) 0.7 else 0.01, Mary)
    State(b,e,a,j,m)
  }

  def main(args:Array[String]) = run

  def run =
    println(p.filter { s:State =>
      !s.e.s && s.j.s && s.m.s && s.a.s
    })
}

