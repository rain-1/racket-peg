.PHONY: install test docs

all:
	echo targets are install update test and docs

install:
	echo 'rm -rf ~/.racket/'
	raco pkg install -t dir --link `pwd`/peg-lib
	raco pkg install -t dir --link `pwd`/peg-doc
	raco pkg install -t dir --link `pwd`/peg

update:
	raco pkg update --link `pwd`/peg-lib
	raco pkg update --link `pwd`/peg-doc
	raco pkg update --link `pwd`/peg

test:
	raco test `pwd`/peg-test

docs:
	scribble --dest peg-doc/scribblings/ peg-doc/scribblings/peg/peg.scrbl
