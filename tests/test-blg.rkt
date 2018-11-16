(use-modules (racket-peg peg) (racket-peg peg-result))
(use-modules (racket-peg rackunit))

;; boolean logic grammar

(define-peg blg (and _ blg-exp-or))
(define-peg/drop _ (* (or #\space #\newline #\tab)))

(define-peg blg-op-or (and "or" _))
(define-peg blg-op-and (and "and" _))
(define-peg blg-bool-true (and "true" _))
(define-peg blg-bool-false (and "false" _))
(define-peg/bake blg-bool (or blg-bool-true blg-bool-false))
(define-peg/tag blg-exp-or (and blg-exp-and (* (and blg-op-or blg-exp-and))))
(define-peg/tag blg-exp-and (and blg-bool (* (and blg-op-and blg-bool))))

(check-equal? (peg blg "true or true and false")
              '((blg-exp-or (blg-exp-and "true") "or" (blg-exp-and "true" "and" "false"))))
(check-equal? (peg blg "true and false")
              '((blg-exp-or (blg-exp-and "true" "and" "false"))))

