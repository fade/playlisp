;;; playlisp.asd - ASDF system definition for playlisp

(asdf:defsystem #:playlisp
  :description "M3U Playlist Parser using Parsnip"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "GNU AFFERO GENERAL PUBLIC LICENSE V.3"
  :version "0.1.0"
  :serial t
  
  :depends-on (#:parsnip
               #:rutils
               #:taglib
               #:alexandria)
  :components ((:file "playlisp")
               (:file "parser")))

(asdf:defsystem #:playlisp/test
  :description "Test suite for playlisp"
  :author "Your Name Here" ; Placeholder, replace if needed
  :license "BSD-3-Clause" ; Placeholder, replace if needed
  :version "0.1.0"
  :depends-on (#:playlisp
               #:parachute)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test :playlisp.test)))
