#lang racket

(require rackunit)

(require "racket-peg.rkt")
(require "peg-sequences.rkt")

;;;;
;; testing it

(define-peg digit (or (char #\0) (call nonzero)))
(define-peg nonzero (range "123456789"))
(define-peg number (or (char #\0)
                       (name n (and (call nonzero) (* (call digit)))))
  (if n (string->number n) 0))
(define-peg pm-number (and (? (name neg (char #\-))) (name n (call number)))
  (if neg (- n) n))

(check-equal? (peg pm-number "0") 0)
(check-equal? (peg pm-number "01") 0)
(check-equal? (peg pm-number "321") 321)
(check-equal? (peg pm-number "-321") -321)
(check-equal? (peg pm-number "100") 100)

(define-peg symbol (and (name s (+ (and (! (char #\space))
                                        (! (char #\())
                                        (! (char #\)))
                                        (any-char))))
                        (* (char #\space)))
  (string->symbol s))
(define-peg symbols (name res (* symbol)) res)
(define-peg sexp (or (and (char #\()
                          (name res (* (call sexp)))
                          (char #\))
                          (* (char #\space)))
                     (name res (call symbol)))
  res)

(check-equal? (peg symbol "foo") 'foo)
(check-equal? (peg symbol "bar baz") 'bar)
(check-equal? (peg symbols "bar baz") '(bar baz))
(check-equal? (peg sexp "()") '())
(check-equal? (peg sexp "(a)") '(a))
(check-equal? (peg sexp "(a b)") '(a b))
(check-equal? (peg sexp "(a b c)") '(a b c))
(check-equal? (peg sexp "(foob (ar baz)quux)") '(foob (ar baz) quux))
(check-equal? (peg sexp "((())(()(())))") '((())(()(()))))
(check-equal? (peg sexp "(((o))(u(u)((e)x))o)") '(((o))(u(u)((e)x))o))
(check-equal? (peg sexp "(lambda (x) (list x (list (quote quote) x)))") '(lambda (x) (list x (list (quote quote) x))))

;(peg sexp "(")
;(peg sexp "(foo")
;(peg sexp "(((foo((((bar(((baz(((")
;(peg sexp "(((foo((((bar(((baz(((quux")

(define-peg +-minus
  (name res (or #\+ #\-))
  (case (string->symbol res)
    ((+) +)
    ((-) -)))
(define-peg expr-sum
  (and (name n1 expr-factor) (? (and (name op +-minus) (name n2 expr-sum))))
  (if n2 (op n1 n2) n1))
(define-peg expr-factor
  (and (name n1 expr-atom) (? (and #\* (name n2 expr-factor))))
  (if n2 (* n1 n2) n1))
(define-peg expr-atom
  (or (and #\( (name res expr-sum) #\))
      (name res pm-number))
  res)

(check-equal? (peg expr-sum "7*(2+3)") 35)
(check-equal? (peg expr-sum "7*2+3") 17)
(check-equal? (peg expr-sum "(7*2)+3") 17)


(define-peg regex-range
  (and #\[ (? (name neg #\^)) (name res (* (or regex-range-range regex-range-single))) #\])
  (if neg `(negate . ,res) res))
(define-peg regex-range-range
  (and (name c1 (any-char)) #\- (name c2 (any-char)))
  `(range ,c1 ,c2))
(define-peg regex-range-single
  (name c1 (and (! #\-) (any-char)))
  `(single ,c1))

(check-equal? (peg regex-range "[a-zA-Z0-9_]")
              '((range "a" "z") (range "A" "Z") (range "0" "9") (single "_")))
(check-equal? (peg regex-range "[^0-9]") '(negate (range "0" "9")))


;; https://www.gnu.org/software/guile/manual/html_node/PEG-Tutorial.html#PEG-Tutorial

(define *etc-passwd*
  "root:x:0:0:root:/root:/bin/bash
daemon:x:1:1:daemon:/usr/sbin:/bin/sh
bin:x:2:2:bin:/bin:/bin/sh
sys:x:3:3:sys:/dev:/bin/sh
nobody:x:65534:65534:nobody:/nonexistent:/bin/sh
messagebus:x:103:107::/var/run/dbus:/bin/false
")

(define-peg passwd (name res (* entry))
  res)
(define-peg/tag entry (and login c pass c uid c gid c name-or-comment c homedir c shell newline+))

(define-peg text (* (and (! #\newline) (! c) (any-char))))
(define-peg/drop c #\:)
(define-peg/drop newline+ (+ #\newline))

(define-peg/tag login text)
(define-peg/tag pass text)
(define-peg/tag uid (* digit))
(define-peg/tag gid (* digit))
(define-peg/tag name-or-comment text)
(define-peg/tag homedir path)
(define-peg/tag shell path)
(define-peg path (* (and (drop #\/) path-element)))
(define-peg path-element (name res (+ (and (! #\/) (! #\newline) (! c) (any-char))))
  res)

(check-equal? (peg passwd *etc-passwd*)
              '((entry
                 (login . "root")
                 (pass . "x")
                 (uid . "0")
                 (gid . "0")
                 (name-or-comment . "root")
                 (homedir "root")
                 (shell "bin" "bash"))
                (entry
                 (login . "daemon")
                 (pass . "x")
                 (uid . "1")
                 (gid . "1")
                 (name-or-comment . "daemon")
                 (homedir "usr" "sbin")
                 (shell "bin" "sh"))
                (entry
                 (login . "bin")
                 (pass . "x")
                 (uid . "2")
                 (gid . "2")
                 (name-or-comment . "bin")
                 (homedir "bin")
                 (shell "bin" "sh"))
                (entry
                 (login . "sys")
                 (pass . "x")
                 (uid . "3")
                 (gid . "3")
                 (name-or-comment . "sys")
                 (homedir "dev")
                 (shell "bin" "sh"))
                (entry
                 (login . "nobody")
                 (pass . "x")
                 (uid . "65534")
                 (gid . "65534")
                 (name-or-comment . "nobody")
                 (homedir "nonexistent")
                 (shell "bin" "sh"))
                (entry
                 (login . "messagebus")
                 (pass . "x")
                 (uid . "103")
                 (gid . "107")
                 (name-or-comment)
                 (homedir "var" "run" "dbus")
                 (shell "bin" "false"))))

(define-peg/tag cfunc (and cSP ctype cSP cname cSP cargs cLB cSP cbody cRB))
(define-peg/tag ctype cidentifier)
(define-peg/tag cname cidentifier)
(define-peg cargs (and cLP (* (and (! (and cSP cRP)) carg cSP (or cCOMMA cRP) cSP)) cSP)) ;; !
(define-peg carg (and cSP ctype cSP cname))
(define-peg/tag cbody (* cstatement))
(define-peg cidentifier (and (or (range "abcdefghijklmnopqrstuvwxyz")
                                 (range "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                             (* (or (range "abcdefghijklmnopqrstuvwxyz")
                                    (range "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                                    (range "0123456789")
                                    #\-))))
(define-peg cstatement (name res (and (* (and (! #\;) (any-char))) cSC cSP))
  res)
(define-peg/drop cSC #\;)
(define-peg/drop cCOMMA #\,)
(define-peg/drop cLP #\()
(define-peg/drop cRP #\))
(define-peg/drop cLB #\{)
(define-peg/drop cRB #\})
(define-peg/drop cSP (* (or #\space #\newline)))

(check-equal? (peg cfunc "int square(int a) { return a*a;}")
              '(cfunc (ctype . "int") (cname . "square") (ctype . "int") (cname . "a") (cbody "return a*a")))
(check-equal? (peg cfunc "int mod(int a, int b) { int c = a/b;return a-b*c; }")
              '(cfunc (ctype . "int") (cname . "mod") (ctype . "int") (cname . "a") (ctype . "int") (cname . "b") (cbody "int c = a/b" "return a-b*c")))


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
              (seq-cat (list (seq-cat '()) (seq-elt '(blg-exp-or (blg-exp-and "true") "or" (blg-exp-and "true" "and" "false"))))))
(check-equal? (peg blg "true and false")
              (seq-cat (list (seq-cat '()) (seq-elt '(blg-exp-or (blg-exp-and "true" "and" "false"))))))

;;; Tiny

(define-peg/bake tiny (and cmd-seq (! (any-char))))
(define-peg cmd-seq (and cmd semicolon (* (and cmd semicolon))))
(define-peg cmd (or if-cmd
                    repeat-cmd
                    assign-cmd
                    read-cmd
                    write-cmd))
(define-peg/tag if-cmd (and "IF" _ expr-sum "THEN" _ cmd-seq (? (and "ELSE" _ cmd-seq)) "END" _))
(define-peg/tag repeat-cmd (and "REPEAT" _ cmd-seq "UNTIL" _ expr-sum))
(define-peg/tag assign-cmd (and symbol _ ":=" _ expr-sum))
(define-peg/tag read-cmd (and "READ" _ symbol))
(define-peg/tag write-cmd (and "WRITE" _ symbol))
(define-peg/drop semicolon (and #\; _))

(define *tiny-example*
  "n := 5;
   f := 1;
   REPEAT
      f := 5*7;
      n := 3-1;
   UNTIL 1;
   WRITE f;")

(check-equal? (peg tiny "n := 5;")
              '((assign-cmd n ":=" 5)))
(check-equal? (peg tiny "f := 5*7;")
              '((assign-cmd f ":=" 35)))
(check-equal? (peg tiny "f := 5*7; WRITE f;")
              '((assign-cmd f ":=" 35)
                (write-cmd "WRITE" f)))
(check-equal? (peg tiny *tiny-example*)
              '((assign-cmd n ":=" 5)
                (assign-cmd f ":=" 1)
                (repeat-cmd "REPEAT" (assign-cmd f ":=" 35) (assign-cmd n ":=" 2) "UNTIL" 1)
                (write-cmd "WRITE" f)))

;;

(define-peg multibrack
  (* (or multibrack-paren
         multibrack-square
         multibrack-brace
         multibrack-angle)))
(define-peg/tag multibrack-paren (and (drop #\() multibrack (drop #\))))
(define-peg/tag multibrack-square (and (drop #\[) multibrack (drop #\])))
(define-peg/tag multibrack-brace (and (drop #\{) multibrack (drop #\})))
(define-peg/tag multibrack-angle (and (drop #\<) multibrack (drop #\>)))

(check-equal? (peg-result->object (peg multibrack "([][{{}}{}]{}[])"))
              '((multibrack-paren (multibrack-square)
                                  (multibrack-square (multibrack-brace (multibrack-brace))
                                                     (multibrack-brace))
                                  (multibrack-brace)
                                  (multibrack-square))))

;;

;JSON ← S? ( Object / Array / String / True / False / Null / Number ) S?
(define-peg json (and s? (or json-object json-array json-string json-true json-false json-null json-number ) s?))

;Object ← "{"
;             ( String ":" JSON ( "," String ":" JSON )*
;             / S? )
;"}"
(define-peg/tag json-object (and (drop "{")
                                 (or (and json-string (drop ":") json (* (and (drop ",") json-string (drop ":") json)))
                                     s?)
                                 (drop "}")))


;Array ← "["
;            ( JSON ( "," JSON )*
;            / S? )
;"]"
(define-peg/tag json-array (and (drop "[")
                                (or (and json (* (and (drop ",") json)))
                                    s?)
                                (drop "]")))

;String ← S? ["] ( [^ " \ U+0000-U+001F ] / Escape )* ["] S?
;Escape ← [\] ( [ " / \ b f n r t ] / UnicodeEscape )
(define-peg/tag json-string (and s?
                                 (drop "\"")
                                 (* (and (! "\"") (any-char)))
                                 (drop "\"")))

(define-peg json-true "true" #t)
(define-peg json-false "false" #f)
(define-peg json-null "null" 'null)

;Number ← Minus? IntegralPart FractionalPart? ExponentPart?
;Minus ← "-"
;IntegralPart ← "0" / [1-9] [0-9]*
;FractionalPart ← "." [0-9]+
;ExponentPart ← ( "e" / "E" ) ( "+" / "-" )? [0-9]+

(define-peg/tag json-number pm-number)

(define-peg/drop s? _)

(check-equal? (peg json-object "{ \"name\": \"John\", \"age\": 30,\"car\":   null }")
              '(json-object (json-string . "name") (json-string . "John") (json-string . "age") (json-number . 30) (json-string . "car") null))

;; couple of tests from http://json.org/example.html

(check-equal? (peg-result->object (peg json
#<<END
{
    "glossary": {
        "title": "example glossary",
		"GlossDiv": {
            "title": "S",
			"GlossList": {
                "GlossEntry": {
                    "ID": "SGML",
					"SortAs": "SGML",
					"GlossTerm": "Standard Generalized Markup Language",
					"Acronym": "SGML",
					"Abbrev": "ISO 8879:1986",
					"GlossDef": {
                        "para": "A meta-markup language, used to create markup languages such as DocBook.",
						"GlossSeeAlso": ["GML", "XML"]
                    },
					"GlossSee": "markup"
                }
            }
        }
    }
}
END
))
  '((json-object
   (json-string . "glossary")
   (json-object
    (json-string . "title")
    (json-string . "example glossary")
    (json-string . "GlossDiv")
    (json-object
     (json-string . "title")
     (json-string . "S")
     (json-string . "GlossList")
     (json-object
      (json-string . "GlossEntry")
      (json-object
       (json-string . "ID")
       (json-string . "SGML")
       (json-string . "SortAs")
       (json-string . "SGML")
       (json-string . "GlossTerm")
       (json-string . "Standard Generalized Markup Language")
       (json-string . "Acronym")
       (json-string . "SGML")
       (json-string . "Abbrev")
       (json-string . "ISO 8879:1986")
       (json-string . "GlossDef")
       (json-object
        (json-string . "para")
        (json-string . "A meta-markup language, used to create markup languages such as DocBook.")
        (json-string . "GlossSeeAlso")
        (json-array (json-string . "GML") (json-string . "XML")))
       (json-string . "GlossSee")
       (json-string . "markup"))))))))

(check-equal?
(peg-result->object (peg json
#<<EOF
{"menu": {
    "header": "SVG Viewer",
    "items": [
        {"id": "Open"},
        {"id": "OpenNew", "label": "Open New"},
        null,
        {"id": "ZoomIn", "label": "Zoom In"},
        {"id": "ZoomOut", "label": "Zoom Out"},
        {"id": "OriginalView", "label": "Original View"},
        null,
        {"id": "Quality"},
        {"id": "Pause"},
        {"id": "Mute"},
        null,
        {"id": "Find", "label": "Find..."},
        {"id": "FindAgain", "label": "Find Again"},
        {"id": "Copy"},
        {"id": "CopyAgain", "label": "Copy Again"},
        {"id": "CopySVG", "label": "Copy SVG"},
        {"id": "ViewSVG", "label": "View SVG"},
        {"id": "ViewSource", "label": "View Source"},
        {"id": "SaveAs", "label": "Save As"},
        null,
        {"id": "Help"},
        {"id": "About", "label": "About Adobe CVG Viewer..."}
    ]
}}
EOF
))
'((json-object
   (json-string . "menu")
   (json-object
    (json-string . "header")
    (json-string . "SVG Viewer")
    (json-string . "items")
    (json-array
     (json-object (json-string . "id") (json-string . "Open"))
     (json-object (json-string . "id") (json-string . "OpenNew") (json-string . "label") (json-string . "Open New"))
     null
     (json-object (json-string . "id") (json-string . "ZoomIn") (json-string . "label") (json-string . "Zoom In"))
     (json-object (json-string . "id") (json-string . "ZoomOut") (json-string . "label") (json-string . "Zoom Out"))
     (json-object (json-string . "id") (json-string . "OriginalView") (json-string . "label") (json-string . "Original View"))
     null
     (json-object (json-string . "id") (json-string . "Quality"))
     (json-object (json-string . "id") (json-string . "Pause"))
     (json-object (json-string . "id") (json-string . "Mute"))
     null
     (json-object (json-string . "id") (json-string . "Find") (json-string . "label") (json-string . "Find..."))
     (json-object (json-string . "id") (json-string . "FindAgain") (json-string . "label") (json-string . "Find Again"))
     (json-object (json-string . "id") (json-string . "Copy"))
     (json-object (json-string . "id") (json-string . "CopyAgain") (json-string . "label") (json-string . "Copy Again"))
     (json-object (json-string . "id") (json-string . "CopySVG") (json-string . "label") (json-string . "Copy SVG"))
     (json-object (json-string . "id") (json-string . "ViewSVG") (json-string . "label") (json-string . "View SVG"))
     (json-object (json-string . "id") (json-string . "ViewSource") (json-string . "label") (json-string . "View Source"))
     (json-object (json-string . "id") (json-string . "SaveAs") (json-string . "label") (json-string . "Save As"))
     null
     (json-object (json-string . "id") (json-string . "Help"))
     (json-object (json-string . "id") (json-string . "About") (json-string . "label") (json-string . "About Adobe CVG Viewer...")))))))
