(use-modules (racket-peg peg) (racket-peg peg-result) (racket-peg s-exp))
(use-modules (racket-peg rackunit))

(check-equal? (peg s-exp
"((null #\\null)
  (backspace #\\backspace)
  (tab #\\tab)
  (newline #\\newline)
  (vtab #\\vtab)
  (page #\\page)
  (return #\\return)
  (space #\\space)
  (rubout #\\rubout))")
  (quote ((null #\nul) (backspace #\backspace) (tab #\tab) (newline #\newline) (vtab #\vtab) (page #\page) (return #\return) (space #\space) (rubout #\rubout))))
