(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

peg-in-peg <-- sp grammar+ ;
grammar <-- (nonterminal ('<--' / '<-' / '<') sp pattern)+ ';' sp ;
pattern <-- alternative (SLASH sp alternative)* ;
alternative <-- ([!&]? sp suffix)+ ;
suffix <-- primary ([*+?] sp)? ;
primary <-- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<' ;
literal <-- ['] (!['] .)* ['] sp ;
charclass <-- LB (!']' (CCrange / CCsingle))* RB sp ;
CCrange <-- . '-' . ;
CCsingle <-- !']' . ;
ntchar <- [a-zA-Z0-9\-] ;
nonterminal <-- ntchar+ !ntchar sp ;
sp < [ \t\n]* ;
SLASH < '/' ;
LB < '[' ;
RB < ']' ;


PEG


(define *expr-peg* #<<EOF
expr <- sum ;expra <- suma ;
sum <-- (product ('+' / '-') sum) / product ;
product <-- (value ('*' / '/') product) / value ;
value <-- number / '(' expr ')' ;
number <-- [0-9]+ ;
EOF
)

(check-equal? (peg peg-in-peg *expr-peg*)
              '(peg-in-peg
                (grammar (nonterminal . "expr") "<-" (pattern (alternative (suffix (primary (nonterminal . "sum"))))) ";")
                (grammar (nonterminal . "expra") "<-" (pattern (alternative (suffix (primary (nonterminal . "suma"))))) ";")
                (grammar
                 (nonterminal . "sum")
                 "<--"
                 (pattern
                  (alternative
                   (suffix
                    (primary
                     "("
                     (pattern
                      (alternative
                       (suffix (primary (nonterminal . "product")))
                       (suffix
                        (primary
                         "("
                         (pattern (alternative (suffix (primary literal . "'+'"))) (alternative (suffix (primary literal . "'-'"))))
                         ")"))
                       (suffix (primary (nonterminal . "sum")))))
                     ")")))
                  (alternative (suffix (primary (nonterminal . "product")))))
                 ";")
                (grammar
                 (nonterminal . "product")
                 "<--"
                 (pattern
                  (alternative
                   (suffix
                    (primary
                     "("
                     (pattern
                      (alternative
                       (suffix (primary (nonterminal . "value")))
                       (suffix
                        (primary
                         "("
                         (pattern (alternative (suffix (primary literal . "'*'"))) (alternative (suffix (primary literal . "'/'"))))
                         ")"))
                       (suffix (primary (nonterminal . "product")))))
                     ")")))
                  (alternative (suffix (primary (nonterminal . "value")))))
                 ";")
                (grammar
                 (nonterminal . "value")
                 "<--"
                 (pattern
                  (alternative (suffix (primary (nonterminal . "number"))))
                  (alternative
                   (suffix (primary literal . "'('"))
                   (suffix (primary (nonterminal . "expr")))
                   (suffix (primary literal . "')'"))))
                 ";")
                (grammar (nonterminal . "number") "<--" (pattern (alternative (suffix (primary charclass (CCrange . "0-9")) "+"))) ";")))
