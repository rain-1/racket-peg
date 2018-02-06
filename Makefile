.PHONY: install test doc

all:
	echo targets are install test and doc

install:
	raco pkg remove peg || true
	raco pkg install --link peg/

test:
	racket test/docs-example-1.rkt
	racket test/docs-example-2.rkt
	racket test/docs-example-3.rkt

	racket test/test-multibrack.rkt
	racket test/test-regex-range.rkt
	racket test/test-etc-passwd.rkt
	racket test/test-cfunc.rkt
	racket test/test-blg.rkt
	racket test/test-tiny.rkt
	racket test/test-json.rkt

	racket test/peg-test-expr.rkt

doc:
	 scribble --dest docs/ docs/peg.scrbl
