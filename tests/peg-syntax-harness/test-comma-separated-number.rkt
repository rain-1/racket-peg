(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

number <-- [0-9]+ ;
file <-- number (~',' number)* ;

PEG

(check-equal? (peg file "123,12,10,21")
              '(file (number . "123") (number . "12") (number . "10") (number . "21")))
