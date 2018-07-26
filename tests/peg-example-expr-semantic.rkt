#lang peg

expr <- sum ;

sum <-- ((v1:product) ('+' / '-') v2:sum) / v3:product --> (if v3 v3 (list 'add v1 v2));

product <-- ((v1:value) ('*' / '/') v2:product) / v3:value --> (if v3 v3 (list 'mult v1 v2));

value <-- number / '(' expr ')' ;

number <- v1:[0-9]+  --> (string->number v1);  //in this way, : binds so that only need parenteses if the pattern is a or
//With semantic actions, the separation between define-peg and define-peg/tag becomes strange. The number
//above, I use what?
