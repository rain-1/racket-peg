#lang racket

(require rackunit)
(require peg)

(require "../peg/peg-example-canopy.rkt")

;; example taken from
;; http://canopy.jcoglan.com/langs/javascript.html

(check-equal? (peg url "http://example.com/search?q=hello#page=1")
              '(url
                (scheme . "http")
                "://"
                (host (hostname (segment . "example") "." (segment . "com")))
                (pathname . "/search")
                (search . "?q=hello")
                (hash . "#page=1")))
