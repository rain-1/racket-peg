# racket-peg

This library implements a PEG parser generator.

## Getting Started

Run `make install` to install the peg library. You should be able to `(require peg/peg)` in your own racket programs after that.

Run `make test` to check that it is working after installation.

## Source Code Map

* `peg/peg.rkt` - The main source file, implementing the PEG language and VM.
* `peg/peg-result.rkt` - machinary for peg results, automatically joining sequences.
* `peg/peg-in-peg.rkt` - defines a special syntax for PEG, using peg itself.
* `peg/main.rkt` - implements the #lang peg glue based on the peg-in-peg parser.
