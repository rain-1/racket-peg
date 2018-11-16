(use-modules (racket-peg peg) (racket-peg peg-result))
(use-modules (racket-peg rackunit))

(define-peg regex-range
  (and #\[ (? (name neg #\^)) (name res (* (or regex-range-range regex-range-single))) #\])
  (if neg `(negate . ,res) res))
(define-peg regex-range-range
  (and (name c1 (any-char)) #\- (name c2 (any-char)))
  `(range ,c1 ,c2))
(define-peg regex-range-single
  (name c1 (and (! #\]) (any-char)))
  `(single ,c1))

(check-equal? (peg regex-range "[a-zA-Z0-9_]")
              '((range "a" "z") (range "A" "Z") (range "0" "9") (single "_")))
(check-equal? (peg regex-range "[^0-9]") '(negate (range "0" "9")))
