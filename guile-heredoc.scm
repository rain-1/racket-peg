(define-module (racket-peg guile-heredoc)
  #:use-module (ice-9 textual-ports)
  #:use-module (racket-peg peg)
  #:use-module (racket-peg peg-in-peg)
  #:use-module (racket-peg peg-to-scheme)
  #:export ())

(define (read-heredoc-literal _ p)
  ;; skip one more < for racket compatability
  (unless (eqv? #\< (read-char p))
    (error 'invalid-heredoc))
  ;; read til the end of line to get our
  (let ((closer (get-line p)))
    ;; read lines until we see our finisher
    (let loop ((lines '()))
      (let ((line (get-line p)))
	(if (or (equal? closer line)
		(eof-object? line))
	    (let ((result (apply string-append (reverse lines))))
	      (if (equal? closer "PEG")
		  ;; it's a PEG heredoc, so parse it and produce some peg source code
		  (peg->scheme (peg peg result))
		  result))
	    (loop (cons (string-append line "\n") lines)))))))

(read-hash-extend #\< read-heredoc-literal)
