# Commands:

.PHONY: build init test clean doc deploy stage prof all

build:
	ghc --make -O -o womancala -main-is Womancala Womancala.hs

prof:
	ghc --make -prof -fprof-auto -o womancala Womancala.hs

test:
	ghc --make -O -o testing -main-is TestCases TestCases.hs
	./testing

all: build test

# Cleaning commands:
clean:
	rm -f womancala test
	rm -f *.hi
	rm -f *.o
	rm -f *.prof
	rm -f testing.exe

setup:
	cabal install ansi-terminal