;; -*-lisp-*-

(uiop:define-package #:playlisp
  (:use #:cl)
  (:use-reexport #:playlisp/parser)
  (:import-from #:playlisp/src/app #:run-tui)
  (:export #:run-tui
           #:-main))

(in-package :playlisp)


;; ==================================================
;;  Entry point
;; ==================================================

(defun -main (&optional args)
  "Launch the playlisp TUI.  Pass an M3U file path as the first argument."
  (let ((filepath (when (consp args) (first args))))
    (run-tui filepath)))

