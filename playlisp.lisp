;; -*-lisp-*-

(uiop:define-package #:playlisp
  (:use #:cl)
  (:use-reexport #:playlisp/parser)
  (:export #:-main))

(in-package :playlisp)


;; ==================================================
;;  Entry point (delegates to McCLIM TUI)
;; ==================================================

(defun -main (&optional args)
  "Launch the playlisp McCLIM TUI.  Pass an M3U file path as the first argument."
  (let ((filepath (when (consp args) (first args))))
    (uiop:symbol-call :playlisp/src/mcclim-app :run filepath)))

