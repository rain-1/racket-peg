#lang racket
(require peg/peg)
(provide (all-defined-out))

(define-peg nt-char (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9) #\_ #\-))
(define-peg/tag nonterminal (and (+ nt-char) (! nt-char) SP))
(define-peg/drop SP (* (or comment (or #\space #\tab #\newline))))
(define-peg/drop comment (and "//" (* (and (! #\newline) (any-char)))))

(define-peg/tag literal
  (and SQ
       (* (or (and BS (or #\' #\\)) (and (! (or #\' #\\)) (any-char))))
       SQ
       SP))
(define-peg/drop SQ #\')
(define-peg/drop BS #\\)

(define-peg/tag charclass (and LB (? "^") (+ (or cc-range cc-escape cc-single)) RB SP))
(define-peg/tag cc-range (and cc-char DASH cc-char))
(define-peg/tag cc-escape (and BS (any-char)))
(define-peg/tag cc-single cc-char)
(define-peg cc-char (and (! cc-escape-char) (any-char)))
(define-peg cc-escape-char (or "[" "]" "-" "^" "\\" "n" "t"))
(define-peg/drop LB "[")
(define-peg/drop RB "]")
(define-peg/drop DASH "-")

(define-peg/tag peg (and SP (+ grammar)))
(define-peg/tag grammar (and (and nonterminal (or "<--" "<-" "<") SP pattern) ";" SP))
(define-peg/tag pattern (and alternative (* (and SLASH SP alternative))))
(define-peg/tag alternative (+ expression))
(define-peg/tag expression (and (? #\!) SP primary (? (and (or #\* #\+ #\?) SP))))
(define-peg/tag primary
  (or (and "(" SP pattern ")" SP)
      (and "." SP)
      literal
      charclass
      nonterminal))
(define-peg/drop SLASH "/")
