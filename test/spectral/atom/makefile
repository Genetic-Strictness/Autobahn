
FAST_ARGS := 1000
MEDI_ARGS := 3000
SLOW_ARGS := 4000

ARGS := $(MED_ARGS)

DIR := $(shell basename `pwd`)

test:
	cabal configure -v0 --enable-profiling
	cabal build -v0
	time cabal run $(DIR) -- $(ARGS)

clean:
	rm -f *.prof *.temp *~ cabal.project.local $(DIR)
	rm -rf autobahn-*
	rm -f minresults
	rm -rf dist-newstyle

all: test

