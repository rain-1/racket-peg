#lang racket

(require rackunit)
(require peg)
(require "peg-syntax/peg-example-shell.rkt")

(check-equal? (peg top-shell "ls -l hello | less") (list (pipe-struct (ls-struct "-l" (dir-or-file "hello")) (less-struct '()))))

(check-equal? (peg top-shell "echo hello world") (list (echo-struct "hello world")))
