#!/bin/sh

set -x

if [[ $# -eq 0 ]]
then
	echo targets are
	echo \* install
	echo \* update
	echo \* bootstrap
	echo \* test
	echo \* docs
else
	case "$1" in
	
		install)
			echo 'rm -rf ~/.racket/'
			raco pkg install -t dir --link `pwd`
			;;

		update)
			raco pkg update --link `pwd`
			;;

		bootstrap)
			raco read -n 256 peg-src/peg-in-peg.peg > peg-in-peg.rkt.tmp
			raco read -n 256 peg-src/s-exp.peg > s-exp.rkt.tmp
			mv peg-in-peg.rkt.tmp peg-in-peg.rkt
			mv s-exp.rkt.tmp s-exp.rkt
			echo make sure to run bootstrap again!
			;;

		test)
			raco test tests/
			;;

		docs)
			scribble --dest scribblings/ scribblings/peg.scrbl
			;;
	esac
fi

