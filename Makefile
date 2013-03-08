
# This Makefile can be used to compile and run the source artifacts
# accompanying the ECOOP'13 paper "Feature-Oriented Programming with
# Object Algebras".

# Copyright (c) 2012-2013
#  - Bruno C.d.S. Oliveira (oliveira@comp.nus.edu.sg)
#  - Tijs van der Storm (storm@cwi.nl)
#  - Alex Loh (alexloh@cs.utexas.edu)
#  - William R. Cook (wcook@cs.utexas.edu)

# Usage:
# make all     : compile the sources (do this before running the examples)
# make grammar : run the grammar case-study
# make stacks  : run the stacks example

# This Makefile has been tested to work with Scala 2.10.1-RC3 on Linux
# (Fedora Core 16 3.4.7-1 x86_64) and Mac OSX 10.7.5 with development
# tools installed.


SRC=src
BIN=bin

ROOT=${SRC}/oalg/algebra


SOURCES=${ROOT}/aspects/Circular.scala \
	${ROOT}/core/Algebras.scala \
	${ROOT}/demo/grammar/Grammar.scala \
	${ROOT}/demo/grammar/Main.scala \
	${ROOT}/demo/stacks/Main.scala \
	${ROOT}/demo/stacks/Stacks.scala \
	${ROOT}/paper/Basic.scala \
	${ROOT}/paper/Generic.scala \
	${ROOT}/paper/Merge.scala \
	${ROOT}/paper/Reflective.scala \
	${ROOT}/paper/Self.scala

all: ${SOURCES}
	scalac -language:higherKinds -classpath ${SRC} -d ${BIN} ${SOURCES}

grammar:
	scala -classpath ${BIN} oalg.algebra.demo.grammar.Main

stacks: 
	scala -classpath ${BIN} oalg.algebra.demo.stacks.Main

clean:
	rm -rf bin/*

