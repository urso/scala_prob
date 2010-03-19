
/*
 * the monty hall problem is a simple game show based probability puzzle with
 * a puzzling outcome :)
 *
 * Suppose you are on a gameshow and you are given the choice of 3 doors. Behind
 * one of these doors is the price and behind the others a goat. Only the
 * moderator knows behind which door the price is and will open one door with a
 * goat after you did your first choice. Next you can choose if you want to 
 * switch doors or not.
 *
 * Question:
 * What is the best strategie? Stay or switch?
 * What are the probabilities of winning for each of these strategies?
 *
 */
object MontyHall {
  import probability.EmbeddedProbability._

  // first we want to encode our state.
  //
  // these are the doors one can choose from: 
  sealed abstract class Door
  object A extends Door { override def toString = "A" }
  object B extends Door { override def toString = "B" }
  object C extends Door { override def toString = "C" }
  val doors = List(A,B,C)

  // the state class used to track the experiments' state
  final case class State(prize:Door,  // door the price is behind
                         chosen:Door, // door currently chosen by player
                         open:Door    // door opened by host
                        )

  // just for the encoding
  sealed abstract class Winning
  object Looser extends Winning { override def toString = "Looser" }
  object Winner extends Winning { override def toString = "Winner" }

  // and a testing function on state to find out if we win or loose
  def testWinner(s:State) = if(s.prize == s.chosen) Winner else Looser

  /*
   * Let us encode the problem with random variables:
   *
   * P  = doors : door prize was put behind
   * C1 = doors : the door choosen in the first round by player
   * O  = doors :  the door opened by show's host
   */

  /*
   *  P(P = A) = 1/3
   *  P(P = B) = 1/3
   *  P(P = C) = 1/3
   */
  def hide = uniform(doors)

  /*
   * and then let the player choose one door:
   * P(C1 = A) = 1/3
   * P(C1 = B) = 1/3
   * P(C2 = C) = 1/3
   */
  def choose = uniform(doors)

  /*
   * and compute probability distribution of host opening a specific door
   * given the event P and C1:
   * P(O|C1,P)
   * with O != C1 and O != P
   */
  def open(hidden:Door, chosen:Door) =
    uniform( doors.filter { x => x != hidden && x != chosen } )

  // play the first round (until game host will open a door)
  def firstRound = {
    val p = hide
    val c = choose
    State(p, c, open(p,c))
  }

  // finally implement strategie 'stay'
  def stay(s:State) = s

  /*
   * and strategy 'switch' where we choose a door C2 with
   * C2 != O and C2 != C1.
   * find P(C2|O, C1, P)
   */
  def switchDoor(s:State) =
    uniform( doors.filter{ x => x != s.open && x != s.chosen}.map { door =>
      State(s.prize, door, s.open)
    })

  def main(args:Array[String]) = run 
  
  // print some results
  def run = {
    println("stay:\n" + prob { testWinner(stay(firstRound)) } + "\n")
    println("switch:\n" + prob { testWinner(switchDoor(firstRound)) })
  }

}

