(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

a <- 'a' -> #f ;

PEG

(check-equal? (peg a "a") #f)

