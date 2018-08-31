#lang peg

top <- v:Tree ! . -> v;
Tree <-- (Branch / SubTree) ~';' ; //will work with ";" as input (peg top ";") will work because that
SubTree <--  Internal / Leaf ; //always work
Leaf <-- Name ; //always work
Name <-- String / '' ; //always work
String <-- [a-zA-Z]+ ;
Internal <-- ~'(' BranchSet ~')' Name ;
BranchSet <-- Branch (~',' Branch)* ;
Branch <-- SubTree Length ;
Length <-- ~':' Number ;
Number <- [0-9]+('.' Number)? ;
