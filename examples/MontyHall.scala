
object MontyHall {
  import probability.EmbeddedProbability._

  sealed abstract class Door
  object A extends Door { override def toString = "A" }
  object B extends Door { override def toString = "B" }
  object C extends Door { override def toString = "C" }

  sealed abstract class Winning
  object Looser extends Winning { override def toString = "Looser" }
  object Winner extends Winning { override def toString = "Winner" }

  val doors = List(A,B,C)

  final case class State(prize:Door, chosen:Door, open:Door)
  def testWinner(s:State) = if(s.prize == s.chosen) Winner else Looser

  def hide = uniform(doors)
  def choose = uniform(doors)
  def open(hidden:Door, chosen:Door) =
    uniform( doors.filter { x => x != hidden && x != chosen } )

  def stay(prize:Door, chosen:Door, open:Door) = State(prize, chosen, open)

  def switchDoor(prize:Door, chosen:Door, open:Door) =
    uniform( doors.filter{ x => x != open && x != chosen}.map { door =>
      State(prize, door, open)
    })

  val strategyStay = prob[Winning] {
    val p = hide
    val c = choose
    testWinner(stay(p, c, open(p, c)))
  }

  val strategySwitch = prob[Winning] {
    val p = hide
    val c = choose
    testWinner(switchDoor(p, c, open(p,c)))
  }

  def main(args:Array[String]) = run 
  
  def run = {
    println("stay:\n" + strategyStay + "\n\n")
    println("switch:\n" + strategySwitch + "\n")
  }

}

