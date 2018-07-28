# racket-peg

This library implements a PEG parser generator.

## Getting Started

Run `make install` to install the peg library. You should be able to `(require peg)` in your own racket programs after that.
If there is a problem installing  with raco pkg, you may be able to just copy the repo to `/usr/share/racket/collect/peg`. This is not recommended but it is a workaround.

Run `make test` to check that it is working after installation.

## Source Code Map

* `peg.rkt` - The main source file, implementing the PEG language and VM.
* `peg-result.rkt` - machinary for peg results, automatically joining sequences.
* `peg-in-peg.rkt` - Implements the standard PEG syntax.
* `peg-in-peg-expanded.rkt` - An expanded version of peg-in-peg.
* `peg-to-scheme.rkt` - translates a parsed AST from the PEG grammar parser to an scheme PEG parser.
* `main.rkt` - implements the `#lang peg` glue based on the peg-in-peg parser.

## Authors

* Raymond Nicholson
* João Pedro Abreu de Souza
