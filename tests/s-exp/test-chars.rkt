#lang racket

(require rackunit)
(require peg)
(require peg/sexp-parser-expanded)

(check-equal? (peg s-exp
"'((null #\\null)
   (backspace #\\backspace)
   (tab #\\tab)
   (newline #\\newline)
   (vtab #\\vtab)
   (page #\\page)
   (return #\\return)
   (space #\\space)
   (rubout #\\rubout))")
   (quote ((null #\nul) (backspace #\backspace) (tab #\tab) (newline #\newline) (vtab #\vtab) (page #\page) (return #\return) (space #\space) (rubout #\rubout))))
