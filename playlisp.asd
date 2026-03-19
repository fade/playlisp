;;; playlisp.asd - ASDF system definition for playlisp

(asdf:register-system-packages "playlisp" '(:playlisp))

(asdf:defsystem #:playlisp
  :class :package-inferred-system
  :description "M3U Playlist Parser using Parsector"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "GNU AFFERO GENERAL PUBLIC LICENSE V.3"
  :version "0.1.0"
  :depends-on (#:parsector
               #:rutils
               #:parse-number
               #:alexandria
               ;; #:playlisp/parser
               #:playlisp/m3u-operations
               )
  :components ((:file "package")
               (:file "playlisp"))
  :in-order-to ((asdf:test-op (asdf:test-op #:playlisp/tests))))

(asdf:defsystem #:playlisp/tests
  :description "Test suite for playlisp"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "GNU AFFERO GENERAL PUBLIC LICENSE V.3"
  :version "0.1.0"
  :depends-on (#:playlisp
               #:playlisp/parser
               #:playlisp/m3u-operations
               #:parachute)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test :playlisp/tests)))

;;; McCLIM-based TUI (requires charmed-mcclim backend)
(asdf:defsystem #:playlisp/mcclim
  :description "McCLIM TUI for playlisp using charmed-mcclim backend"
  :author "Glenn Thompson"
  :license "GNU AFFERO GENERAL PUBLIC LICENSE V.3"
  :version "0.1.0"
  :depends-on (#:parse-number
               #:playlisp/parser
               #:playlisp/m3u-operations
               #:mcclim-charmed)
  :components ((:module "src"
                :components ((:file "mcclim-app")))))
