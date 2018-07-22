#lang peg

bncn <-- ('b' 'c') / ('b' bncn 'c') ;
anbn <-- ('a' 'b') / ('a' anbn 'b') ;

equal-b-c <-- 'a'+ bncn ;
equal-a-b <-- anbn 'c'+ ;

anbncn <-- (& equal-b-c) equal-a-b ;
