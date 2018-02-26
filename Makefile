.PHONY: install test docs

all:
	echo targets are install test and docs

install:
	raco pkg remove peg || true
	raco pkg install --link

test:
	raco test tests

docs:
	 scribble --dest scribblings/ scribblings/peg.scrbl
