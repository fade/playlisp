;;; src/ansi.lisp - Pure ANSI terminal control for playlisp TUI
;;; Inspired by gilt/clabber ANSI layers, written fresh for playlisp

(uiop:define-package #:playlisp/src/ansi
  (:use #:cl)
  (:export ;; Escape
           #:*escape*
           ;; Colors
           #:fg #:bg #:fg-rgb #:bg-rgb #:reset
           #:bold #:dim #:italic #:underline #:inverse
           #:with-style
           ;; Cursor
           #:cursor-to #:cursor-home
           #:cursor-up #:cursor-down
           #:cursor-forward #:cursor-back
           #:cursor-hide #:cursor-show
           ;; Screen
           #:clear-screen #:clear-line #:clear-to-end
           #:begin-sync-update #:end-sync-update
           ;; Color palette
           #:register-color #:color-code))

(in-package #:playlisp/src/ansi)

;;; ── Escape character ──────────────────────────────────────────────

(defparameter *escape* (code-char 27))

;;; ── Color palette ─────────────────────────────────────────────────

(defparameter *color-palette* (make-hash-table :test 'eq))

(defun register-color (name index)
  "Register a named color with its 256-color index."
  (setf (gethash name *color-palette*) index))

(defun color-code (name-or-index)
  "Resolve a color name keyword or integer to a 256-color index."
  (etypecase name-or-index
    (integer name-or-index)
    (keyword (or (gethash name-or-index *color-palette*) 7))))

;; Standard palette
(register-color :black 0)
(register-color :red 1)
(register-color :green 2)
(register-color :yellow 3)
(register-color :blue 4)
(register-color :magenta 5)
(register-color :cyan 6)
(register-color :white 7)
(register-color :bright-black 8)
(register-color :bright-red 9)
(register-color :bright-green 10)
(register-color :bright-yellow 11)
(register-color :bright-blue 12)
(register-color :bright-magenta 13)
(register-color :bright-cyan 14)
(register-color :bright-white 15)
;; Playlisp-specific
(register-color :header 75)
(register-color :track-num 245)
(register-color :duration 140)
(register-color :selected 33)
(register-color :border 245)
(register-color :border-active 75)
(register-color :status 236)

;;; ── Color output ──────────────────────────────────────────────────

(defun fg (color)
  "Set foreground color. COLOR is a keyword name or 256-color index."
  (format *terminal-io* "~C[38;5;~Dm" *escape* (color-code color)))

(defun bg (color)
  "Set background color. COLOR is a keyword name or 256-color index."
  (format *terminal-io* "~C[48;5;~Dm" *escape* (color-code color)))

(defun fg-rgb (r g b)
  "Set foreground to 24-bit true color."
  (format *terminal-io* "~C[38;2;~D;~D;~Dm" *escape* r g b))

(defun bg-rgb (r g b)
  "Set background to 24-bit true color."
  (format *terminal-io* "~C[48;2;~D;~D;~Dm" *escape* r g b))

;;; ── Text attributes ───────────────────────────────────────────────

(defun bold ()      (format *terminal-io* "~C[1m" *escape*))
(defun dim ()       (format *terminal-io* "~C[2m" *escape*))
(defun italic ()    (format *terminal-io* "~C[3m" *escape*))
(defun underline () (format *terminal-io* "~C[4m" *escape*))
(defun inverse ()   (format *terminal-io* "~C[7m" *escape*))
(defun reset ()     (format *terminal-io* "~C[0m" *escape*))

(defmacro with-style ((&key fg bg bold dim italic underline inverse) &body body)
  "Execute BODY with specified text styling, then reset."
  `(progn
     ,@(when bold '((bold)))
     ,@(when dim '((dim)))
     ,@(when italic '((italic)))
     ,@(when underline '((underline)))
     ,@(when inverse '((inverse)))
     ,@(when fg `((fg ,fg)))
     ,@(when bg `((bg ,bg)))
     ,@body
     (reset)))

;;; ── Cursor control ────────────────────────────────────────────────

(defun cursor-to (row col)
  (format *terminal-io* "~C[~D;~DH" *escape* (max 1 row) (max 1 col)))

(defun cursor-home ()
  (format *terminal-io* "~C[H" *escape*))

(defun cursor-up (&optional (n 1))
  (format *terminal-io* "~C[~DA" *escape* n))

(defun cursor-down (&optional (n 1))
  (format *terminal-io* "~C[~DB" *escape* n))

(defun cursor-forward (&optional (n 1))
  (format *terminal-io* "~C[~DC" *escape* n))

(defun cursor-back (&optional (n 1))
  (format *terminal-io* "~C[~DD" *escape* n))

(defun cursor-hide ()
  (format *terminal-io* "~C[?25l" *escape*))

(defun cursor-show ()
  (format *terminal-io* "~C[?25h" *escape*))

;;; ── Screen control ────────────────────────────────────────────────

(defun clear-screen ()
  (format *terminal-io* "~C[2J~C[H" *escape* *escape*))

(defun clear-line ()
  (format *terminal-io* "~C[2K" *escape*))

(defun clear-to-end ()
  (format *terminal-io* "~C[K" *escape*))

(defun begin-sync-update ()
  "Begin synchronized update — terminal buffers until end-sync-update."
  (format *terminal-io* "~C[?2026h" *escape*))

(defun end-sync-update ()
  "End synchronized update — terminal flushes buffered content."
  (format *terminal-io* "~C[?2026l" *escape*))
