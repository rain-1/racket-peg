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
	raco pkg install -t dir --link `pwd`

update:
	raco pkg update --link `pwd`

bootstrap:
	raco read -n 256 peg-src/peg-in-peg.peg > peg-in-peg-expanded.rkt.tmp
	raco read -n 256 peg-src/s-exp.peg > s-exp.rkt.tmp
	mv peg-in-peg-expanded.rkt.tmp peg-in-peg-expanded.rkt
	mv s-exp.rkt.tmp sexp-parser-expanded.rkt
	@echo make sure to run bootstrap again!

test:
	raco test tests/

docs:
	scribble --dest scribblings/ scribblings/peg.scrbl
