(module anything racket
  (provide (all-defined-out))
  (require peg/peg)
  (begin
    (define-peg/drop _ (* (or #\space #\tab #\newline)))
    (define-peg s-exp (or list quote quasiquote unquote boolean number identifier string))
    (define-peg list (and "(" _ (name lst (* (and s-exp _))) ")") lst)
    (define-peg quote (and "'" _ (name s s-exp)) (list 'quote s))
    (define-peg quasiquote (and "`" _ (name s s-exp)) (list 'quasiquote s))
    (define-peg unquote (and "," _ (name s s-exp)) (list 'unquote s))
    (define-peg boolean (or (name x "#t") (name x "#f")) (equal? "#t" x))
    (define-peg identifier (name s (+ (and (! (or #\space #\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\# #\| #\\)) (any-char)))) (string->symbol s))
    (define-peg number (name n (+ (range #\0 #\9))) (string->number n))
    (define-peg string (and #\" (name s (* (or (and (! (or #\" #\\)) (any-char)) (and (drop #\\) (any-char))))) #\") s)))
