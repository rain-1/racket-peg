(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

float <-- '-'? [0-9]+ ('.' [0-9]+)? ;

// based on https://notes.kartashov.com/2016/01/30/writing-a-simple-query-language-with-antlr/

_ <- [ ]* ;

OP < '(' _ ;
CL < ')' _ ;
CM < ',' _ ;

location <-- OP float CM float CL ;
amount <-- float _ unit ;
unit <- 'km' _ / '%' _ ;

expression <- predicate 'and' _ expression
	    / predicate 'or' _ expression
            / predicate ;
predicate <-- 'location' _ 'within' _ amount 'from' _ location
           / 'status.stateOfCharge' '<' _ amount ;

PEG


(check-equal? (peg expression "location within 10 km from (-37.814, 144.963) and status.stateOfCharge < 10%")
              '(predicate "location within " (amount (float . "10") " km ") "from " (location (float . "-37.814") (float . "144.963"))))
