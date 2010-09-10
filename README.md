
Introduction
------------

sb_probdsl offers simple discrete probabilistic programming support using scala's
new delimited continuations support.

Installation alias preparing Playground
----------------------------------------

For compiling scala_probdsl you will need [sbt](http://code.google.com/p/simple-build-tool).

Further library dependencies to install using "sbt publish-local" before
compiling scala_probdsl:

- [scala_mprob](http://github.com/urso/scala_mprob)
- [embeddedmonads](http://github.com/urso/embeddedmonads)

compile scala_probdsl:

    sbt compile

Usage:
------

See Examples
TODO:

Evaluation Strategies:
----------------------
**TODO: this section is a little outdated. better see source**

Instead of computing the probability distributions directly an unevaulated
decision tree is build and only the root is returned.

On that tree different evaluation strategies may be applied. Implemented so
far are:


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

In order to use the examples in the scala REPL, you just need to load them 
into the scala console startet using "sbt console" and call the example its
"run" method:

    $ sbt console
    scala> :load examples/MontyHall.scala
    Loading examples/MontyHall.scala...
    defined module MontyHall
    scala> MontyHall.run
    ...

    scala> :load examples/DrugTest.scala
    Loading examples/DrugTest.scala...
    defined module DrugTest

    scala> DrugTest.run
    ...
    
Recommended example reading order:

- examples/Test1.scala   # just some very basic experiments
- examples/Diagnosis.scala  # most basic bayesian inference example
- examples/MontyHall.scala  # monty hall problem/paradox
- examples/Alarm.scala      # example from Artificial Intelligence - A Modern Approach
- example/SpamPlan.scala    # a (not complete) spam filter 

