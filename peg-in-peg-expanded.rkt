#lang racket
(require peg/peg)
(provide (all-defined-out))

(define-peg nt-char (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9) #\-))
(define-peg/tag nonterminal (and (+ (call nt-char)) (! (call nt-char)) (call SP)))
(define-peg/drop SP (* (or #\space #\tab #\newline)))

(define-peg/tag literal (and (call SQ) (* (or (and (call BS) (or #\' #\\)) (and (! (or #\' #\\)) (any-char)))) (call SQ) SP))
(define-peg/drop SQ (or #\'))
(define-peg/drop BS (or #\\))

(define-peg/tag charclass (and (call LB) (? "^") (+ (or (call cc-single) (call cc-escape) (call cc-range))) (call RB) (call SP)))
(define-peg cc-char (and (! (call cc-escape-char)) (any-char)))
(define-peg cc-escape-char (or "[" "]" "-" "^" "\\" "n" "t"))
(define-peg/tag cc-single (call cc-char))
(define-peg/tag cc-escape (and BS (any-char)))
(define-peg/tag cc-range (and (call cc-char) (call DASH) (call cc-char)))
(define-peg/drop LB "[")
(define-peg/drop RB "]")
(define-peg/drop DASH "-")

(define-peg/tag peg (and (call SP) (+ (call grammar))))
(define-peg/tag grammar (and (and (call nonterminal) (or "<--" "<-" "<") (call SP) (call pattern)) ";" (call SP)))
(define-peg/tag pattern (and (call alternative) (* (and (call SLASH) (call SP) (call alternative)))))
(define-peg/tag alternative (+ expression))
(define-peg/tag expression (and (? (or #\!)) (call SP) (call primary) (? (and (or #\* #\+ #\?) (call SP)))))
(define-peg/tag primary
  (or (and "(" (call SP) (call pattern) ")" (call SP)) (and "." (call SP)) (call literal) (call charclass) (call nonterminal)))
(define-peg/drop SLASH "/")
