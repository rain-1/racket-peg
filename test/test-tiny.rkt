#lang racket

(require rackunit)

(require peg/peg)
(require peg/peg-result)

;;; Tiny

(define-peg/drop _ (* (or #\space #\tab #\newline)))
(define-peg number (name res (+ (range #\0 #\9)))
 (string->number res))
(define-peg symbol
  (and (name res (+ (and (! #\( #\) #\space #\newline) (any-char)))) _)
  (string->symbol res))

(define-peg/bake tiny (and cmd-seq (! (any-char))))
(define-peg cmd-seq (and cmd semicolon (* (and cmd semicolon))))
(define-peg cmd (or if-cmd
                    repeat-cmd
                    assign-cmd
                    read-cmd
                    write-cmd))
(define-peg/tag if-cmd (and "IF" _ number "THEN" _ cmd-seq (? (and "ELSE" _ cmd-seq)) "END" _))
(define-peg/tag repeat-cmd (and "REPEAT" _ cmd-seq "UNTIL" _ number))
(define-peg/tag assign-cmd (and symbol _ ":=" _ number))
(define-peg/tag read-cmd (and "READ" _ symbol))
(define-peg/tag write-cmd (and "WRITE" _ symbol))
(define-peg/drop semicolon (and #\; _))

(define *tiny-example*
  "n := 5;
   f := 1;
   REPEAT
      f := 35;
      n := 2;
   UNTIL 1;
   WRITE f;")

(check-equal? (peg tiny "n := 5;")
              '((assign-cmd n ":=" 5)))
(check-equal? (peg tiny "f := 35;")
              '((assign-cmd f ":=" 35)))
(check-equal? (peg tiny "f := 35; WRITE f;")
              '((assign-cmd f ":=" 35)
                (write-cmd "WRITE" f)))
(check-equal? (peg tiny *tiny-example*)
              '((assign-cmd n ":=" 5)
                (assign-cmd f ":=" 1)
                (repeat-cmd "REPEAT" (assign-cmd f ":=" 35) (assign-cmd n ":=" 2) "UNTIL" 1)
                (write-cmd "WRITE" f)))
