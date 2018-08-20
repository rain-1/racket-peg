#lang racket

(require rackunit)

(require peg)

(define-peg u-chr (char #\λ))
(check-equal? (peg u-chr "λ")
	      "λ")
(check-equal? (peg (any-char) "λ")
	      "λ")

(check-equal? (peg (* (range #\ぁ #\ゖ)) "こんにちはhello")
	      "こんにちは")

(check-equal? (peg (string "こんにちは") "こんにちはhello")
	      "こんにちは")
