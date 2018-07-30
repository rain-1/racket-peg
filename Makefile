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
	raco pkg install -t dir --link `pwd`/peg

update:
	raco pkg update --link `pwd`/peg

bootstrap:
	racket peg-src/expand-lang.rkt peg-src/peg-in-peg.rkt > peg-in-peg-expanded.rkt
	racket peg-src/expand-lang.rkt peg-src/sexp-parser.rkt > sexp-parser-expanded.rkt
	mv peg-in-peg-expanded.rkt peg/peg-in-peg-expanded.rkt
	mv sexp-parser-expanded.rkt peg/sexp-parser-expanded.rkt
	@echo make sure to run bootstrap again!

test:
	raco test tests/

docs:
	scribble --dest scribblings/ scribblings/peg/peg.scrbl
