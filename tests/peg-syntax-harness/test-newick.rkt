#lang racket

(require peg)
(require rackunit)

(require "../peg-syntax/peg-example-newick.rkt")

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
