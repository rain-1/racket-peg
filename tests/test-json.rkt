#lang racket

(require rackunit)

(require peg/peg)
(require peg/peg-result)


(define-peg/drop _ (* (or #\space #\tab #\newline)))

(define-peg digit (or (char #\0) (call nonzero)))
(define-peg nonzero (range #\1 #\9))
(define-peg number (or (char #\0)
                       (name n (and (call nonzero) (* (call digit)))))
  (if n (string->number n) 0))
(define-peg pm-number (and (? (name neg (char #\-))) (name n (call number)))
(if neg (- n) n))



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
