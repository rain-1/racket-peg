#lang scribble/manual
 
@title{PEG}

This library implements a PEG parser generator.
 
@section{Introduction}

PEG can be thought of as an advance over regex. It can match more languages (for example balanced brackets) and can be paired with semantic actions to produce structured results from a parse.

The PEG language is implemented as a system of macros that compiles parser descriptions (rules) into scheme code. It is also provided with a custom syntax via @racket{#lang peg}.

The generated code parses text by interacting with the "PEG VM", which is a set of registers holding the input text, input position, control stack for backtracking and error reporting notes.

@section{Syntax Reference}

@subsection{define-peg}

@defform[(define-peg name rule)
  #:grammar ([<rule>
              (code:line (epsilon) (code:comment "always succeeds"))
	      (code:line (char c) (code:comment "matches the character c"))
	      (code:line (any-char) (code:comment "matches any character"))
	      (code:line (range c1 c2) (code:comment "match any char between c1 and c2"))
	      (code:line (string str) (code:comment "matches string str"))
	      (code:line (and <rule> ...) (code:comment "sequence"))
	      (code:line (or <rule> ...) (code:comment "prioritized choice"))
     	      (code:line (* <rule> ...) (code:comment "zero or more"))
     	      (code:line (+ <rule> ...) (code:comment "one or more"))
     	      (code:line (? <rule> ...) (code:comment "zero or one"))
     	      (code:line (call name))
     	      (code:line (capture name <rule>))
     	      (code:line (! <rule> ...) (code:comment "negative lookahead"))
     	      (code:line (drop <rule> ...) (code:comment "discard the semantic result on matching"))
	      ])]{
Defines a new scheme function named @racket[peg-rule:name] by compiling the peg rule into scheme code that interacts with the PEG VM.
}

@defform[(define-peg name rule action)]{
Same as above, but also performs a semantic action to produce its result. Semantic actions are regular scheme expressions, they can refer to variables named by a @racket[capture].
}

We also provide shorthands for some common semantic actions:

@defform[(define-peg/drop name rule)]{
@code{= (define-peg rule-name (drop rule))}

makes the parser produce no result.
}

@defform[(define-peg/bake name rule)]{
@code{= (define-peg rule-name (name res rule) res)}

transforms the peg-result into a scheme object.
}

@defform[(define-peg/tag name rule)]{
@code{= (define-peg rule-name (name res exp) `(rule-name . ,res))}

tags the result with the peg rule name. Useful for parsers that create an AST.
}

@subsection{peg}

@defform[(peg rule input-text)]{
Run a PEG parser. Attempt to parse the @racket[input-text] string using the given @racket[rule]. This is sets up the PEG VM registers into an initial state and then calls into the parser for @racket[rule].
}

@section{Examples}

@subsection{Example 1}

For a simple example, lets try splitting a sentence into words. We can describe a word as one or more of non-space characters, optionally followed by a space:

@codeblock{
> (require peg/peg)
> (define *sentence* "the quick brown fox jumps over the lazy dog")
> (define-peg non-space
    (and (! #\space) (any-char)))
> (define-peg/bake word
    (and (+ non-space)
         (drop (? #\space))))
> (peg word *sentence*)
"the"
> (peg (+ word) *sentence*)
'("the" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog")
}

@subsection{Example 2}

Here is a simple calculator example that demonstrates semantic actions and recursive rules.

@codeblock{
(define-peg number (name res (+ (range #\0 #\9)))
  (string->number res))
(define-peg sum
  (and (name v1 prod) (? (and #\+ (name v2 sum))))
  (if v2 (+ v1 v2) v1))
(define-peg prod
  (and (name v1 number) (? (and #\* (name v2 prod))))
  (if v2 (* v1 v2) v1))
}

Usage:

@codeblock{
> (peg sum "2+3*4")
14
> (peg sum "2*3+4")
10
> (peg sum "7*2+3*4")
26
}

@subsection{Example 3}

Here is an example of parsing balanced paranthesis. It demonstrates a common idiom of using @racket[_] for skipping whitespace, and using @racket{define-peg/bake} to produce a list rather than a sequence from a @racket[*].

@codeblock{
(define-peg/drop _ (* (or #\space #\newline)))

(define-peg symbol
  (and (name res (+ (and (! #\( #\) #\space #\newline) (any-char)))) _)
  (string->symbol res))

(define-peg/bake sexp
  (or symbol
      (and (drop #\() (* sexp) (drop #\) _))))
}

Usage:

@codeblock{
> (peg sexp "(foob (ar baz)quux)")
'(foob (ar baz) quux)
> (peg sexp "((())(()(())))")
'((()) (() (())))
> (peg sexp "(lambda (x) (list x (list (quote quote) x)))")
'(lambda (x) (list x (list 'quote x)))
}
