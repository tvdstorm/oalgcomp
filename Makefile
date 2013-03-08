
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

SRC=src
BIN=bin

all:
	scalac -language:higherKinds -classpath ${SRC} -d ${BIN}/ `find ${SRC} -name "*.scala"`

grammar:
	scala -classpath ${BIN} oalg.algebra.demo.grammar.Main

stacks: 
	scala -classpath ${BIN} oalg.algebra.demo.stacks.Main

clean:
	rm -rf bin/*

