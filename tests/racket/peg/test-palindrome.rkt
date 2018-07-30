#lang racket

(require rackunit)
(require peg)

(define-peg marked-palindrome
  (or "m"
      (and "1" marked-palindrome "1")
      (and "0" marked-palindrome "0")))

(check-equal? (peg marked-palindrome "100m001") "100m001")
(check-equal? (peg marked-palindrome "101m101") "101m101")
(check-equal? (peg marked-palindrome "000001m100000") "000001m100000")
(check-exn exn:fail? (lambda () (peg marked-palindrome "010101m010101")))
