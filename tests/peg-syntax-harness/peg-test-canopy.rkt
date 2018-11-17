(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

url       <--  scheme '://' host pathname search hash? ;
scheme    <--  'http' 's'? ;
host      <--  hostname port? ;
hostname  <--  segment ('.' segment)* ;
segment   <--  [a-z0-9\-]+ ;
port      <--  ':' [0-9]+ ;
pathname  <--  '/' [^ ?]* ;
search    <--  ('?' [^ #]*)? ;
hash      <--  '#' [^ ]* ;

PEG

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
