;;; src/layout.lisp - Dual-pane layout manager for playlisp TUI
;;; Computes panel positions from terminal dimensions

(uiop:define-package #:playlisp/src/layout
  (:use #:cl
        #:playlisp/src/ansi
        #:playlisp/src/terminal
        #:playlisp/src/widgets)
  (:export #:layout
           #:make-layout
           #:layout-compute
           #:layout-render-all
           #:layout-tracklist
           #:layout-details
           #:layout-status-y
           #:layout-width
           #:layout-height))

(in-package #:playlisp/src/layout)

;;; ── Layout class ──────────────────────────────────────────────────

(defclass layout ()
  ((tracklist :accessor layout-tracklist :initform nil
              :documentation "Left pane: tracklist panel")
   (details   :accessor layout-details   :initform nil
              :documentation "Right pane: track details panel")
   (status-y  :accessor layout-status-y  :initform 1
              :documentation "Row for the status bar")
   (width     :accessor layout-width     :initform 80)
   (height    :accessor layout-height    :initform 24)
   (split-ratio :initarg :split-ratio :accessor layout-split-ratio
                :initform 0.6
                :documentation "Fraction of width allocated to the tracklist pane"))
  (:documentation "Dual-pane layout: tracklist on left, details on right"))

(defun make-layout (&key (split-ratio 0.6))
  (make-instance 'layout :split-ratio split-ratio))

(defun layout-compute (layout width height)
  "Compute panel positions for the given terminal dimensions.
   Layout:  row 2 .. (height-1): dual pane (row 1 reserved for top margin)
            row height: status bar"
  (setf (layout-width layout) width
        (layout-height layout) height)
  (let* ((status-h 1)
         (top-margin 1)
         (pane-h (- height status-h top-margin))
         (left-w (max 20 (floor (* width (layout-split-ratio layout)))))
         (right-w (- width left-w)))
    (setf (layout-status-y layout) height)
    ;; Left pane: tracklist (start at row 2 to avoid row 1 clipping)
    (setf (layout-tracklist layout)
          (make-instance 'tracklist-panel
                         :x 1 :y (1+ top-margin)
                         :width left-w :height pane-h
                         :active t))
    ;; Right pane: details
    (setf (layout-details layout)
          (make-instance 'details-panel
                         :x (1+ left-w) :y (1+ top-margin)
                         :width right-w :height pane-h
                         :title "Track Details")))
  layout)

;;; ── Status bar rendering ──────────────────────────────────────────

(defun render-status-bar (layout &key mode filename track-count total-duration)
  "Render the bottom status bar."
  (let ((y (layout-status-y layout))
        (w (layout-width layout)))
    (cursor-to y 1)
    (bg :status)
    (fg :white)
    ;; Left: mode indicator
    (let ((mode-str (or mode "NORMAL")))
      (bold)
      (format *terminal-io* " ~A " mode-str)
      (reset)
      (bg :status)
      (fg :white))
    ;; Center: file info with duration
    (let ((info (format nil "~@[~A~]~@[ · ~D tracks~]~@[ · ~A~]"
                        filename track-count
                        (when total-duration (format-runtime total-duration)))))
      (princ info *terminal-io*))
    ;; Pad to end
    (let* ((used (+ 10 (length (or filename "")) 25))
           (pad (max 0 (- w used))))
      (princ (make-string pad :initial-element #\Space) *terminal-io*))
    ;; Right: help hint
    (fg :cyan)
    (princ "q:quit ?:help " *terminal-io*)
    (reset)))

;;; ── Full render ───────────────────────────────────────────────────

(defun layout-render-all (layout &key mode filename track-count total-duration)
  "Render all panels and the status bar."
  (begin-sync-update)
  (cursor-to 1 1)  ; Ensure we start at top-left
  (when (layout-tracklist layout)
    (panel-render (layout-tracklist layout)))
  (when (layout-details layout)
    (panel-render (layout-details layout)))
  (render-status-bar layout
                     :mode mode
                     :filename filename
                     :track-count track-count
                     :total-duration total-duration)
  (end-sync-update)
  (force-output *terminal-io*))
