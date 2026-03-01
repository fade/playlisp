;; -*-lisp-*-

(uiop:define-package #:playlisp
  (:use #:cl)
  (:use-reexport #:playlisp/parser)
  (:export nil))

(in-package :playlisp)



;; ==================================================
;;  Parsing the playlist data
;; ==================================================

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

