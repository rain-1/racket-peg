#lang peg

(require "../list-monad.rkt");

text <- str:[^{},]+ -> (list str);
items <- xs:(text / choice)+ -> (map string-concatenate (sequence xs));
choice <- '{' x:items xs:(~',' items)* '}' -> (concatenate (cons x xs));
