all:
	echo targets are install test and doc

install:
	raco pkg remove peg || true
	raco pkg install --link peg/

test:
	racket examples/docs-example-1.rkt
	racket examples/docs-example-2.rkt
	racket examples/docs-example-3.rkt
	racket examples/test-multibrack.rkt
	racket examples/test-regex-range.rkt
	racket examples/test-etc-passwd.rkt
	racket examples/test-cfunc.rkt
	racket examples/test-blg.rkt
	racket examples/test-tiny.rkt
	racket examples/test-json.rkt

doc:
	 scribble --dest docs/ docs/peg.scrbl
