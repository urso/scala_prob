
Introduction
------------

sb_prob offers simple discrete probabilistic programming support using scala's
new (experimental) delimited continuations support.

Installation alias preparing Playground
----------------------------------
It is Assumed you are using the official 'Scala 2.8.0 Beta 1' release.
Unfortunately continuations are not included in this beta and you have to
build it on your own:

- Follow continuations plugin install description found at
  http://blog.richdougherty.com/2010/01/scala-280-beta-1-released.html.

- INFO: do 'svn up -r 20706' after checking out the plugin and before building

- Get the scala monadic probabilistic programming library from
  http://github.com/urso/scala_mprob

- build scala_mprob jar file with 'buildr package'

- open Makefile and adjust path for the variables
  "CPS_PLUGIN", "CPS_LIB" and "PROB_MONAD_LIB"

- build library:

    $ make build

- (optionally) build examples:
    
    $ make

Usage:
------

See Examples
TODO:

Evaluation Strategies:
----------------------

Instead of computing the probability distributions directly an unevaulated
decision tree is build and only the root is returned.

On that tree different evaluation strategies may be applied. Implemented so
far are:

    runProbabilistic[A] { ... } // returns unevaluated tree

    prob[A] { ... } // will evaluate tree to full decision tree

    normalizedProb[A] { ... } // like prob[A], but will apply
                              // Probability.normalize to distribution
                              // which must have the type 
                              // Distribution[Option[A]].
                              // usefull when doing bayesian inference

    pickValue[A](tree)      // randomly samples a value from unevaluated tree.
                            // This is linear in the number of random
                            // variables to be visited.

    collect[A](pred, tree)  // logically samples values from unevaluated tree
                            // until the given predicate returns false.
                            // Due to the fact, that the tree is build lazily
                            // sampling a value from tree is O(N) with N being
                            // the number random variables to visit.

                            
    collecting[A](pred) { ... } // uses collect to evaluate given context

    loopK(k), loopMaxMs(time)   // predefined predicates to be used with
                                // collect/collecting evaluators

Some examples can be found in "examples/Test1.scala"

Examples:
---------

In order to use the examples in the scala REPL, you have to compile them first
and then start the REPL using make:

    $ make # compile examples
    $ make repl # run scala repl with correct classpath

In the REPL you can run the examples by typing "<example>.run" (well, you
should know how to use the REPL).
Try:

    > Diagnosis.run
    ...
    > MontyHall.run
    ...
    > SpamPlan.run

    
Recommended example reading order:

- examples/Test1.scala   # just some very basic experiments
- examples/Diagnosis.scala  # most basic bayesian inference example
- examples/MontyHall.scala  # monty hall problem/paradox
- examples/Alarm.scala      # example from Artificial Intelligence - A Modern Approach
- example/SpamPlan.scala    # a (not complete) spam filter 

