
SCALA = scala
SCALAC = scalac

CPS_PLUGIN = $(SCALA_HOME)/lib/selectivecps-plugin.jar
CPS_LIB = $(HOME)/tmp/continuation/build/build.library

PROB_MONAD_LIB = ../prob/target/probability-0.0.1.jar

.PHONY: clean repl build test

test:
	[ -e build ] || mkdir build
	$(SCALAC) -Xplugin:$(CPS_PLUGIN) -cp $(CPS_LIB):$(PROB_MONAD_LIB):. -d build -deprecation -unchecked `find examples -name '*.scala'`

build:
	$(SCALAC) -Xplugin:$(CPS_PLUGIN) -cp $(CPS_LIB):$(PROB_MONAD_LIB)  `find src -name '*.scala'`

clean:
	rm -fR build
	rm -fR probability

repl:
	$(SCALA) -cp $(CPS_LIB):build:$(PROB_MONAD_LIB):.

