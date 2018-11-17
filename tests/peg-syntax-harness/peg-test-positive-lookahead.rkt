(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

bncn <-- ('b' 'c') / ('b' bncn 'c') ;
anbn <-- ('a' 'b') / ('a' anbn 'b') ;

equal-b-c <-- 'a'+ bncn ;
equal-a-b <-- anbn 'c'+ ;

anbncn <-- (& equal-b-c) equal-a-b ;

PEG

(check-equal? (peg anbncn "aaabbbccc")
  '(anbncn (equal-a-b (anbn "a" (anbn "a" (anbn . "ab") "b") "b") "ccc")))

(check-equal? (peg anbncn "abc")
  '(anbncn (equal-a-b (anbn . "ab") "c")))

