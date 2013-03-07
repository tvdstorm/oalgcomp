
SRC=src
BIN=bin

all:
	scalac -language:higherKinds -classpath ${SRC} -d ${BIN}/ `find ${SRC} -name "*.scala"`

clean:
	rm -rf bin/*

