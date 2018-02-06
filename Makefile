all:
	echo targets are install test and doc

install:
	raco pkg remove peg || true
	raco pkg install --link peg/

test:
	racket examples/docs-example-1.rkt
	racket examples/docs-example-2.rkt
	racket examples/docs-example-3.rkt

doc:
	 scribble --dest docs/ docs/peg.scrbl
