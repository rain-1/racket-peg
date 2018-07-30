.PHONY: install test docs

all:
	@echo targets are
	@echo \* install
	@echo \* update
	@echo \* bootstrap
	@echo \* test
	@echo \* docs

install:
	echo 'rm -rf ~/.racket/'
	raco pkg install -t dir --link `pwd`/peg-lib
	raco pkg install -t dir --link `pwd`/peg-doc
	raco pkg install -t dir --link `pwd`/peg

update:
	raco pkg update --link `pwd`/peg-lib
	raco pkg update --link `pwd`/peg-doc
	raco pkg update --link `pwd`/peg

bootstrap:
	racket peg-src/expand-lang.rkt peg-src/peg-in-peg.rkt > peg-in-peg-expanded.rkt
	racket peg-src/expand-lang.rkt peg-src/sexp-parser.rkt > sexp-parser-expanded.rkt
	mv peg-in-peg-expanded.rkt peg-lib/peg/peg-in-peg-expanded.rkt
	mv sexp-parser-expanded.rkt peg-lib/peg/sexp-parser-expanded.rkt

test:
	raco test `pwd`/peg-test

docs:
	scribble --dest peg-doc/scribblings/ peg-doc/scribblings/peg/peg.scrbl
