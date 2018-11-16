(use-modules (racket-peg peg) (racket-peg peg-result))
(use-modules (racket-peg rackunit))

;; https://www.gnu.org/software/guile/manual/html_node/PEG-Tutorial.html#PEG-Tutorial

(define *etc-passwd*
  "root:x:0:0:root:/root:/bin/bash
daemon:x:1:1:daemon:/usr/sbin:/bin/sh
bin:x:2:2:bin:/bin:/bin/sh
sys:x:3:3:sys:/dev:/bin/sh
nobody:x:65534:65534:nobody:/nonexistent:/bin/sh
messagebus:x:103:107::/var/run/dbus:/bin/false
")

(define-peg digit (range #\0 #\9))

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
