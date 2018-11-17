(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

number <- value:[0-9]+ -> (string->number value);
sum <- v1:number ~'+' v2:number -> `(+ ,v1 ,v2);

PEG

(check-equal?
  (peg sum  "123+12")
  '(+ 123 12))
