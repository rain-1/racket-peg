.PHONY: install test docs

all:
	echo targets are install update test and docs

install:
	raco pkg install -t dir --link `pwd`

update:
	raco pkg update --link `pwd`

test:
	raco test tests

docs:
	 scribble --dest scribblings/ scribblings/peg.scrbl
