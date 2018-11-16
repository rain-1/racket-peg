(use-modules (racket-peg peg) (racket-peg peg-result))
(use-modules (racket-peg rackunit))

(define-peg u-chr (char #\λ))
(check-equal? (peg u-chr "λ")
	      "λ")
(check-equal? (peg (any-char) "λ")
	      "λ")

(check-equal? (peg (* (range #\ぁ #\ゖ)) "こんにちはhello")
	      "こんにちは")

(check-equal? (peg (string "こんにちは") "こんにちはhello")
	      "こんにちは")
