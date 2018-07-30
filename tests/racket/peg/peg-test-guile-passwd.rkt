#lang racket

(require rackunit)
(require peg)

(require "peg-example-guile-passwd.rkt")

(define *etc-passwd*
  "root:x:0:0:root:/root:/bin/bash
daemon:x:1:1:daemon:/usr/sbin:/bin/sh
bin:x:2:2:bin:/bin:/bin/sh
sys:x:3:3:sys:/dev:/bin/sh
nobody:x:65534:65534:nobody:/nonexistent:/bin/sh
messagebus:x:103:107::/var/run/dbus:/bin/false
")

(check-equal? (peg passwd *etc-passwd*)
              '(passwd
                (entry
                 (login . "root")
                 (pass . "x")
                 (uid . "0")
                 (gid . "0")
                 (nameORcomment . "root")
                 (homedir path (pathELEMENT . "root"))
                 (shell path (pathELEMENT . "bin") (pathELEMENT . "bash")))
                (entry
                 (login . "daemon")
                 (pass . "x")
                 (uid . "1")
                 (gid . "1")
                 (nameORcomment . "daemon")
                 (homedir path (pathELEMENT . "usr") (pathELEMENT . "sbin"))
                 (shell path (pathELEMENT . "bin") (pathELEMENT . "sh")))
                (entry
                 (login . "bin")
                 (pass . "x")
                 (uid . "2")
                 (gid . "2")
                 (nameORcomment . "bin")
                 (homedir path (pathELEMENT . "bin"))
                 (shell path (pathELEMENT . "bin") (pathELEMENT . "sh")))
                (entry
                 (login . "sys")
                 (pass . "x")
                 (uid . "3")
                 (gid . "3")
                 (nameORcomment . "sys")
                 (homedir path (pathELEMENT . "dev"))
                 (shell path (pathELEMENT . "bin") (pathELEMENT . "sh")))
                (entry
                 (login . "nobody")
                 (pass . "x")
                 (uid . "65534")
                 (gid . "65534")
                 (nameORcomment . "nobody")
                 (homedir path (pathELEMENT . "nonexistent"))
                 (shell path (pathELEMENT . "bin") (pathELEMENT . "sh")))
                (entry
                 (login . "messagebus")
                 (pass . "x")
                 (uid . "103")
                 (gid . "107")
                 (nameORcomment)
                 (homedir path (pathELEMENT . "var") (pathELEMENT . "run") (pathELEMENT . "dbus"))
                 (shell path (pathELEMENT . "bin") (pathELEMENT . "false")))))
