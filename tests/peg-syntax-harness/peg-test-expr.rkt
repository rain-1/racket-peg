(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

expr <- sum ;
sum <-- (product ('+' / '-') sum) / product ;
product <-- (value ('*' / '/') product) / value ;
value <-- number / '(' expr ')' ;
number <-- [0-9]+ ;

PEG

(check-equal? (peg expr "4324+431")
              '(sum (product value number . "4324") "+" (sum product value number . "431")))
(check-equal? (peg expr "72*4324+431")
	      '(sum (product (value number . "72") "*" (product value number . "4324"))
                    "+" (sum product value number . "431")))
(check-equal? (peg expr "4324+72*431")
	      '(sum (product value number . "4324") "+"
                    (sum product (value number . "72") "*" (product value number . "431"))))
(check-equal? (peg expr "4324+72*431*27")
	      '(sum (product value number . "4324") "+"
                    (sum product (value number . "72") "*"
                         (product (value number . "431") "*" (product value number . "27")))))

