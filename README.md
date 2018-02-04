# racket-peg

# Questions

## How do I use this?

The subdirectory `peg/` is a racket package that needs to be installed.

```
raco pkg install --link peg/
```

Once it is installed you can make use it with `(require peg/peg)`.

You can remove it with:

```
raco pkg remove peg
```

## why does path-element do (define-peg path-element (name res ...) res)?

`(define-peg name p)`

expands to

(A) `(define-peg name p #f)`

it is not the same as

(B) `(define-peg name (name res p) res)`

the difference is that (A) produces a peg-result. and since all captured matches are converted from peg-results to scheme values (B) produces a different sort of value.

with the (A) definition path would parse "/bin/bash" with result "binbash", and (B) gives result ("bin" "bash")
