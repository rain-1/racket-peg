(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

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

PEG

(check-equal?
  (peg top "((B:0.2,(C:0.3,D:0.4)E:0.5)A:0.1)F;")
  '(Tree
    (SubTree
    Internal
    (BranchSet
      (Branch
      (SubTree
       Internal
        (BranchSet
        (Branch (SubTree Leaf Name String . "B") (Length . "0.2"))
        (Branch
          (SubTree Internal (BranchSet (Branch (SubTree Leaf Name String . "C") (Length . "0.3")) (Branch (SubTree Leaf Name String . "D") (Length . "0.4"))) (Name String . "E"))
          (Length . "0.5")))
        (Name String . "A"))
       (Length . "0.1")))
     (Name String . "F"))))
