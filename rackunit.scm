(define-module (racket-peg rackunit)
  #:export (check-equal?))

(define-syntax check-equal?
  (syntax-rules ()
    ((check-equal? x y)
     (let ((x-val x)
	   (y-val y))
       (unless (equal? x-val y-val)
	 (write `(test failed ,x not equal to ,y))
	 (newline)
	 
	 (write `(got ,x-val expected ,y-val))
	 (newline)
	 
	 (exit 5))))))
