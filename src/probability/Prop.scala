
package probability

import scala.continuations._
import scala.continuations.ControlContext._

object EmbeddedProbability {
  import Utils._

  abstract sealed class PStateTree[A] {
    def reify : Distribution[A]
    def pick : A
  }

  case class PChoice[A](d: () => Distribution[PStateTree[A]]) extends PStateTree[A] {
    def reify = d().dep { m => reifyDistribution(m) }
    def pick  = d().pick._1.pick
  }

  case class PValue[A](value:A) extends PStateTree[A] {
    def reify = Probability.single[A](value)
    def pick  = value
  }

  case class PNone[A]() extends PStateTree[Option[A]] {
    def reify = Probability.single[Option[A]](None)
    def pick  = None
  }

  val PNull = PNone()

  def reifyDistribution[A](p:PStateTree[A]) : Distribution[A] = p.reify
  def pickValue[A](p:PStateTree[A]) : A = p.pick

  def runProb_[A,C](ctx: => (A @cps[PStateTree[A],PStateTree[C]])) : PStateTree[C] =
      reset {
        PValue(ctx);
      }

  def runProbabilistic[A](ctx: => (Any @cps[PStateTree[Any],PStateTree[Any]])) : PStateTree[A] =
      runProb_(ctx).asInstanceOf[PStateTree[A]]

  def normalizedProb_[A,C](ctx: => (A @cps[PStateTree[A],PStateTree[Option[C]]])) : Distribution[C] =
    Probability.normalize(prob_[A,Option[C]](ctx))

  def normalizedProb[C](ctx: => (Any @cps[PStateTree[Any],PStateTree[Any]])) : Distribution[C] =
    Probability.normalize(prob[Option[C]](ctx))

  def prob_[A,C](ctx: => (A @cps[PStateTree[A],PStateTree[C]])) : Distribution[C] =
    reifyDistribution(runProb_[A,C](ctx))

  def prob[C](ctx: => (Any @cps[PStateTree[Any],PStateTree[Any]])) : Distribution[C] =
      prob_(ctx).asInstanceOf[Distribution[C]]

  def collect[A](pred: () => Boolean, p:PStateTree[A]) : Distribution[A] = {
    import scala.collection._
    new Distribution(
      block1(new mutable.HashMap[A,Double]) { m =>
        while(pred()) {
          val x = pickValue(p)
          m += x -> (m.getOrElse(x, 0.0) + 1.0)
        }
      })
  }

  def loopK(k:Int) : (() => Boolean) = {
    var tmp = k;
    { () => val r = tmp > 0; tmp -= 1; r }
  }

  def loopMaxMs(k:Long) : (() => Boolean) = {
    import java.lang.System

    val start = System.currentTimeMillis()
    return {() => 
      (System.currentTimeMillis() - start) < k
    }
  }

  def collecting_[A,C](pred: () => Boolean) :
    (( => A @cps[PStateTree[A],PStateTree[C]]) => Distribution[C]) = 
  { ctx =>
    collect(pred, runProb_(ctx))
  }

  def collecting[A](pred: () => Boolean) :
    (( => Any @cps[PStateTree[Any],PStateTree[Any]]) => Distribution[A]) =
  {
    ctx =>
    collect(pred, runProbabilistic(ctx)).asInstanceOf[Distribution[A]]
  }

  def guard(b:Boolean) : Unit @cps[PStateTree[Any], PStateTree[Any]]  = if (!b) {
      shift { k:(Unit => PStateTree[Any]) => PNull.asInstanceOf[PStateTree[Any]] }
  }

  def dist_[A,B](d:Iterable[(Double, A)]) : A @cps[PStateTree[B], PStateTree[B]] =
    shift { k: (A => PStateTree[B]) =>
        PChoice{ () =>
          Distribution(d.map {x:(Double, A) =>
            val tmp = k(x._2)
            (x._1, tmp)
          })
        }
    }

  def dist_[A,B](d:Distribution[A]) : A @cps[PStateTree[B], PStateTree[B]] =
    shift { k: (A => PStateTree[B]) =>
      PChoice{ () =>
        Distribution(d.iterator.map { x:(A, Double) =>
          (x._2, k(x._1))
        })
      }
    }

  def dist[A](d:Iterable[(Double, A)]) : A @cps[PStateTree[Any],PStateTree[Any]] = dist_(d)
  def dist[A](d:Distribution[A]) : A @cps[PStateTree[Any],PStateTree[Any]] = dist_(d)
  def dist[A](d:(Double,A)*) : A @cps[PStateTree[Any],PStateTree[Any]] = dist_(d)

  def uniform[A](d:Iterable[A]) : A @cps[PStateTree[Any],PStateTree[Any]] =
    dist(d.map { x:A => (1.0, x) })

  def flip(x:Double) : Boolean  @cps[PStateTree[Any],PStateTree[Any]] =
    flip(x,true, false)

  def flip[A](x:Double, a:A, b:A) : A @cps[PStateTree[Any],PStateTree[Any]] = 
    dist[A]((x, a), (1.0 - x, b))

}

