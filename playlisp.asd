;; -*-lisp-*-
;;;; playlisp.asd

(asdf:defsystem #:playlisp
  :description "utilities to parse and generate m3u playlists"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Modified BSD License"
  :serial t
  :depends-on (:alexandria
               :rutils
               :str
               :cl-ppcre
               :taglib
               :cl-flac
               :parsnip)
  :pathname "./"
  :components ((:file "app-utils")
               (:file "packages")
               (:file "parse2-m3u")
               (:file "playlisp")
               (:file "tests")))

