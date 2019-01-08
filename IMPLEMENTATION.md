# This document describes the internal workings of the PEG parser.

## Writing PEGs

There are 2 ways to make use of the PEG parser:

* (A) by writing your PEG parsing code in s-expressions with the `peg` scheme macro.
* (B) by writing your PEG parser code in peg syntax using `#lang peg`.

(B) is implemented by transforming the peg code into version (A) s-expressions, and this is done using a PEG parser.

## PEG VM execution

The execution of a PEG parser on an input string is done using a virtual machine. The state or the registers of the virtual machine as well as all the helper functions which operate directly on them are in `peg.rkt` prefixed with `pegvm-`.

They store things the input text, the current position in the input text, the alternatives available for backtracking, whether we are inside a negation (which stops peg variables being bound), as well as some extra stuff that is noted down for producing better error messages.

## PEG compilation

The core of the system is the `peg-compile` macro, this transforms a s-expression based PEG parser description into CPS code (continuation passing style), it takes a success continuation as a parameter. So any valid parse will put into result into `sk`, allowing `sk` to return it or to continue parsing and combining that result piece with others.

Let's look at some specific parts of the implementation with commentary. Hopefully this will help make the system of continuations we used to implement the parser understandable by example.

### (epsilon)

```
      [(epsilon)
       #'(sk empty-sequence)]
```

In this `(epsilon)` rule, parsing always succeeds so we simply call the success continuation straight away.

### (and e1 e2)

```
      [(and e1 e2)
       (with-syntax ([p1 (peg-compile #'e1 #'mk)]
                     [p2 (peg-compile #'e2 #'sk^)])
         #'(let ((stack-reset (unbox (pegvm-control-stack))))
             (let ((mk (lambda (r1)
                         (let ((sk^ (lambda (r2)
                                      (sk (peg-result-join r1 r2)))))
                           (set-box! (pegvm-control-stack) stack-reset)
                           p2))))
               p1)))]
```

In `(and e1 e2)` we produce code that first tries to parse `e1`. On success it puts the result of that parse into the `mk` continuation named as `r1`. If it failed then a parse error would returned straight away and `mk` would never be called.

After `e1` parsed successfully we reset the control stack so that any extra alternatives that showed up during the parsing of `e1` are discarded, basically we do a commit to this first successful parse of `e1`. This was introduced to improve efficiency, but users do need to be aware of it.

Then we attempt to parse the second part of the `and` expression `e2`, putting the result value into `sk^` as `r2`. If that worked we simply invoke `sk` - the ultimate success continuation with the two result objects joined together.

### (or e1 e2)

```
      [($or e1 e2)
       (with-syntax ([p1 (peg-compile #'e1 #'sk)]
                     [p2 (peg-compile #'e2 #'sk)])
         #'(begin (pegvm-push-alternative! (lambda () p2))
                  p1))]
      [(or e1 e2)
       (with-syntax ([p (peg-compile #'($or e1 e2) #'sk)])
         #'(parameterize ([pegvm-current-choice '(or e1 e2)])
             p))]
```

In its simplest form the `or` just leaves the parsing of `e2` as an alternative option in case `e1` fails. The PEG VM will attempt to execute alternatives in the event of a failure.

The implementation of `or` was split into two parts at some point though, so that we could take a note of the fact a branch was taken in order to produce improved error messages.

### (! e)

Negation is a rather interesting one to watch, as it basically flips around the success and failure continuations.

