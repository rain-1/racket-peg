.PHONY: install test docs

all:
	echo targets are install test and docs

install:
	raco pkg remove peg || true
	raco pkg install --link

test:
	racket tests/docs-example-1.rkt
	racket tests/docs-example-2.rkt
	racket tests/docs-example-3.rkt

	racket tests/test-multibrack.rkt
	racket tests/test-regex-range.rkt
	racket tests/test-etc-passwd.rkt
	racket tests/test-cfunc.rkt
	racket tests/test-blg.rkt
	racket tests/test-tiny.rkt
	racket tests/test-json.rkt

	racket tests/peg-test-expr.rkt

docs:
	 scribble --dest scribblings/ scribblings/peg.scrbl
