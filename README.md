# racket-peg

This library implements a PEG parser generator.

## Getting Started

* run `chmod +x ./make` to turn ./make executable

* Run `./make install` to install the peg library. You should be able to `(require peg)` in your own racket programs after that. Or use `#lang peg`!

* Run `./make update` to apply changes, if you're hacking on it.

* Run `./make docs` to build the documentation.

* Run `./make test` to check that it is working after installation.

## Source Code Map

The PEG parser system is built in two stages. There is a lispy s-expression version. Then there is a self-hosted PEG syntax version.

The lispy version is implemented in:

* `peg.rkt` - The PEG parsing VM and parser macro.
* `peg-result.rkt` - The fundamental data structure used for parse results. It's a kind of automatically joinable sequence.

The PEG aspect is implemented in these files:

* `peg-src/peg-in-peg.rkt` - The syntax of our peg language. In peg.
* `peg-src/sexp-parser.rkt` - A basic s-expression parser. In peg.
 
Both of the above files are "bootstrapped" using the racket macro expander to produce the following:

* `peg-in-peg.rkt` - expanded version of `peg-src/peg-in-peg.peg`.
* `s-exp.rkt` - expanded version of `peg-src/s-exp.peg`.
* `peg-to-scheme.rkt` - support for peg-in-peg.
* `main.rkt` - This adds the `#lang peg` glue to racket.

## Authors

* Raymond Nicholson
* João Pedro Abreu de Souza
