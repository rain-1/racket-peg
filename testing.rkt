#lang racket

(require "racket-peg.rkt")

;;;;
;; testing it

(define-peg digit (choice (char #\0) (call nonzero)))
(define-peg nonzero (range "123456789"))
(define-peg number (choice (char #\0)
                           (name n (seq (call nonzero) (star (call digit)))))
  (if n (string->number n) 0))
(define-peg pm-number (seq (optional (name neg (char #\-))) (name n (call number)))
  (if neg (- n) n))
;> (peg pm-number "0")
;parse successful! 0
;> (peg pm-number "01")
;parse successful! 0
;> (peg pm-number "321")
;parse successful! 321
;> (peg pm-number "-321")
;parse successful! -321
;> (peg pm-number "100")
;parse successful! 100

(define-peg symbol (seq (name s (plus (seq (not (char #\space))
                                           (not (char #\())
                                           (not (char #\)))
                                           (any-char))))
                        (star (char #\space)))
  (string->symbol s))
(define-peg sexp (choice (seq (char #\()
                              (name res (star (call sexp)))
                              (char #\))
                              (star (char #\space)))
                         (name res (call symbol)))
  res)
;> (peg sexp "(foob (ar baz)quux)")
;parse successful! (foob (ar baz) quux)

(define-peg plus-minus
  (name res (choice #\+ #\-))
  (case (string->symbol res)
    ((+) +)
    ((-) -)))
(define-peg expr-sum
  (seq (name n1 expr-factor) (optional (seq (name op plus-minus) (name n2 expr-sum))))
  (if n2 (op n1 n2) n1))
(define-peg expr-factor
  (seq (name n1 expr-atom) (optional (seq #\* (name n2 expr-factor))))
  (if n2 (* n1 n2) n1))
(define-peg expr-atom
  (choice (seq #\( (name res expr-sum) #\))
          (name res pm-number))
  res)
;> (peg expr-sum "7*(2+3)")
;parse successful! 35
;35
;> (peg expr-sum "7*2+3")
;parse successful! 17
;17
;> (peg expr-sum "(7*2)+3")
;parse successful! 17
;17


(define-peg regex-range
  (seq #\[ (optional (name neg #\^)) (name res (star (choice regex-range-range regex-range-single))) #\])
  (if neg `(negate ,res) res))
(define-peg regex-range-range
  (seq (name c1 (any-char)) #\- (name c2 (any-char)))
  `(range ,c1 ,c2))
(define-peg regex-range-single
  (name c1 (seq (not #\-) (any-char)))
  `(single ,c1))

;> (peg regex-range "[a-zA-Z0-9_]")
;parse successful! ((range "a" "z") (range "A" "Z") (range "0" "9") (single "_"))
;'((range "a" "z") (range "A" "Z") (range "0" "9") (single "_"))
;> (peg regex-range "[^0-9]")
;parse successful! (negate (range "0" "9"))
;'(negate (range "0" "9"))
