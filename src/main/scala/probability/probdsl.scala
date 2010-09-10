package probability

import scala.util.continuations._
import scalaz._
import Monad.monad

import embeddedmonads._

object DeferedDistributionPure extends Pure[DeferedDistribution] {
    def pure[A](a: => A) = defered.single[A](a)
}

object DeferedDistributionBind extends Bind[DeferedDistribution] {
    def bind[A,B](a: DeferedDistribution[A], f: A => DeferedDistribution[B]) = 
        a flatMap f
}

private object EmbeddedDistribution extends EmbeddedMonad[DeferedDistribution]()(
    monad[DeferedDistribution](DeferedDistributionBind, DeferedDistributionPure)) 

object probdsl {
    import EmbeddedDistribution._

    def runProb[C](ctx: => Any @cpsParam[DeferedDistribution[Any], 
                                         DeferedDistribution[Any]]) 
                  : DeferedDistribution[C] =
        runMonad[C](ctx)

    def prob[C](ctx: => Any @cpsParam[DeferedDistribution[Any], 
                                      DeferedDistribution[Any]]) 
               : Distribution[Option[C]] = 
        runProb[C](ctx).reify

    def normalizedProb[C](ctx: => Any @cpsParam[DeferedDistribution[Any], 
                                                DeferedDistribution[Any]]) 
                         : Distribution[C] =
        runProb[C](ctx).normalizedProb

    def sampleOne[C](ctx: => Any @cpsParam[DeferedDistribution[Any],
                                           DeferedDistribution[Any]]) 
                    : Option[C] =
        runProb[C](ctx).pick

    def collect[C](pred: () => Boolean)
                  (ctx: => Any @cpsParam[DeferedDistribution[Any],
                                         DeferedDistribution[Any]]) 
                  : Distribution[Option[C]] =
        runProb[C](ctx).collect(pred)

    def normalizedCollect[C](pred: () => Boolean)
                            (ctx: => Any @cpsParam[DeferedDistribution[Any], 
                                                   DeferedDistribution[Any]]) 
                            : Distribution[C] =
        runProb[C](ctx).normalizedCollect(pred)

    def loopK(k:Int) : (() => Boolean) = defered.loopK(k)

    def loopMaxMs(k:Long) : (() => Boolean) = defered.loopMaxMs(k)

    def flip[A](x:Double, a:A, b:A) : A @ cpsParam[DeferedDistribution[Any], 
                                                   DeferedDistribution[Any]] = 
        defered.flip[A](x,a,b).value

    def flip(x:Double) : Boolean @ cpsParam[DeferedDistribution[Any], 
                                            DeferedDistribution[Any]] = 
        flip[Boolean](x, true, false)

    def uniform[A](d:Iterable[A]) : A @ cpsParam[DeferedDistribution[Any],
                                                 DeferedDistribution[Any]] = 
        defered.uniform(d).value

    def linear[A](d:Iterable[A]) : A @ cpsParam[DeferedDistribution[Any],
                                                DeferedDistribution[Any]] = 
        defered.linear(d).value

    def single[A](a:A) : A @cpsParam[DeferedDistribution[Any],
                                     DeferedDistribution[Any]] = 
        defered.single(a).value

    def dist[A](d:Distribution[A]) : A @cpsParam[DeferedDistribution[Any],
                                                 DeferedDistribution[Any]] = 
        defered.dist[A](d).value

    def dist[A](d:Iterable[(Double, A)]) : A @cpsParam[DeferedDistribution[Any],
                                                       DeferedDistribution[Any]] = 
        dist[A](Distribution(d))

    def dist[A](d:(Double,A)*) : A @cpsParam[DeferedDistribution[Any],
                                             DeferedDistribution[Any]] =
        dist[A](Distribution(d))

    def guard(b:Boolean) : Unit @cpsParam[DeferedDistribution[Any],
                                          DeferedDistribution[Any]] = if (!b) {
        shift { k: (Unit => DeferedDistribution[Any]) =>
            defered.pnull[Any]
        }
    }

    def stop : Unit @ cpsParam[DeferedDistribution[Any],
                               DeferedDistribution[Any]] = guard(false)
}

