;;; src/widgets.lisp - Panel widgets for playlisp TUI
;;; Base panel class, tracklist panel, track details panel

(uiop:define-package #:playlisp/src/widgets
  (:use #:cl #:playlisp/src/ansi #:playlisp/parser)
  (:export ;; Base panel
           #:panel
           #:panel-x #:panel-y #:panel-width #:panel-height
           #:panel-title #:panel-active-p #:panel-dirty-p
           #:panel-render
           #:draw-box #:clear-panel-content
           ;; Tracklist panel
           #:tracklist-panel
           #:tracklist-panel-playlist
           #:tracklist-panel-cursor
           #:tracklist-panel-scroll-offset
           #:tracklist-visible-height
           ;; Details panel
           #:details-panel
           #:details-panel-track
           ;; File browser panel
           #:file-browser-panel
           #:browser-current-dir
           #:browser-entries
           #:browser-cursor
           #:browser-scroll-offset
           #:browser-visible-height
           #:browser-refresh
           #:browser-selected-entry
           #:browser-entry-name
           #:browser-entry-dir-p
           #:browser-entry-path
           #:browser-entry-selected-p
           #:browser-toggle-selection
           #:browser-selected-files
           #:browser-selection-count
           #:browser-recursive-p
           #:*audio-extensions*
           #:*music-library-path*
           #:truncate-string
           #:format-runtime))

(in-package #:playlisp/src/widgets)

;;; ── Box-drawing characters ────────────────────────────────────────

(defparameter *box-tl* #\┌)
(defparameter *box-tr* #\┐)
(defparameter *box-bl* #\└)
(defparameter *box-br* #\┘)
(defparameter *box-h*  #\─)
(defparameter *box-v*  #\│)

;;; ── Base panel class ──────────────────────────────────────────────

(defclass panel ()
  ((x      :initarg :x      :accessor panel-x      :initform 1)
   (y      :initarg :y      :accessor panel-y      :initform 1)
   (width  :initarg :width  :accessor panel-width  :initform 40)
   (height :initarg :height :accessor panel-height :initform 10)
   (title  :initarg :title  :accessor panel-title  :initform nil)
   (active-p :initarg :active :accessor panel-active-p :initform nil)
   (dirty-p  :initform t     :accessor panel-dirty-p))
  (:documentation "Base panel widget with position, size, border, and title"))

(defgeneric panel-render (panel)
  (:documentation "Render the panel to the terminal."))

(defun draw-box (x y w h &key title active-p)
  "Draw a Unicode box border at (X, Y) with dimensions W x H."
  (let ((border-color (if active-p :border-active :border)))
    ;; Top border - ensure we're at correct position
    (cursor-to y x)
    (force-output *terminal-io*)
    (fg border-color)
    (princ *box-tl* *terminal-io*)
    (loop repeat (- w 2) do (princ *box-h* *terminal-io*))
    (princ *box-tr* *terminal-io*)
    (force-output *terminal-io*)
    ;; Now overlay title if present
    (when title
      (let* ((max-tw (- w 4))
             (display (if (> (length title) max-tw)
                          (subseq title 0 max-tw)
                          title)))
        (cursor-to y (+ x 2))
        (when active-p (bold))
        (fg :white)
        (princ display *terminal-io*)
        (reset)
        (fg border-color)))
    ;; Side borders
    (loop for row from (1+ y) below (+ y h -1) do
      (cursor-to row x)
      (princ *box-v* *terminal-io*)
      (cursor-to row (+ x w -1))
      (princ *box-v* *terminal-io*))
    ;; Bottom border
    (cursor-to (+ y h -1) x)
    (princ *box-bl* *terminal-io*)
    (loop repeat (- w 2) do (princ *box-h* *terminal-io*))
    (princ *box-br* *terminal-io*)
    (reset)))

(defun clear-panel-content (x y w h)
  "Clear the interior area of a panel."
  (loop for row from (1+ y) below (+ y h -1) do
    (cursor-to row (1+ x))
    (princ (make-string (- w 2) :initial-element #\Space) *terminal-io*)))

;;; ── Tracklist panel ───────────────────────────────────────────────

(defclass tracklist-panel (panel)
  ((playlist      :initarg :playlist      :accessor tracklist-panel-playlist
                  :initform nil)
   (cursor        :initarg :cursor        :accessor tracklist-panel-cursor
                  :initform 0
                  :documentation "Index of currently selected track (0-based)")
   (scroll-offset :initarg :scroll-offset :accessor tracklist-panel-scroll-offset
                  :initform 0
                  :documentation "First visible track index"))
  (:documentation "Panel displaying a playlist's track list with selection cursor"))

(defun tracklist-visible-height (panel)
  "Number of track rows visible inside the panel border."
  (- (panel-height panel) 2))

(defun format-runtime (seconds)
  "Format SECONDS as M:SS or H:MM:SS. Returns \"--:--\" for NIL."
  (cond
    ((null seconds) "--:--")
    ((not (numberp seconds)) "--:--")
    ((< seconds 0) "--:--")
    ((>= seconds 3600)
     (multiple-value-bind (h rest) (floor seconds 3600)
       (multiple-value-bind (m s) (floor rest 60)
         (format nil "~D:~2,'0D:~2,'0D" h m (round s)))))
    (t
     (multiple-value-bind (m s) (floor seconds 60)
       (format nil "~D:~2,'0D" m (round s))))))

(defun truncate-string (s max-len)
  "Truncate S to MAX-LEN characters, appending … if truncated."
  (if (<= (length s) max-len)
      s
      (concatenate 'string (subseq s 0 (max 0 (- max-len 1))) "…")))

(defmethod panel-render ((panel tracklist-panel))
  (let* ((x (panel-x panel))
         (y (panel-y panel))
         (w (panel-width panel))
         (h (panel-height panel))
         (playlist (tracklist-panel-playlist panel))
         (tracks (when playlist (playlist-elements playlist)))
         (cursor (tracklist-panel-cursor panel))
         (offset (tracklist-panel-scroll-offset panel))
         (vis-h (tracklist-visible-height panel))
         (title (or (panel-title panel)
                    (when playlist (playlist-name playlist))
                    "Tracklist")))
    ;; Draw border
    (draw-box x y w h :title title :active-p (panel-active-p panel))
    ;; Clear content area
    (clear-panel-content x y w h)
    ;; Render tracks
    (when tracks
      (let ((content-w (- w 4))       ; interior minus padding
            (num-w 4)                  ; width for track number
            (dur-w 7))                 ; width for duration
        (loop for i from 0 below vis-h
              for track-idx = (+ offset i)
              while (< track-idx (length tracks))
              do (let* ((track (nth track-idx tracks))
                        (row (+ y 1 i))
                        (selected-p (= track-idx cursor))
                        (title-w (max 1 (- content-w num-w dur-w 2)))
                        (display-title (or (title track)
                                           (file-namestring (track-path track))
                                           "???"))
                        (display-num (format nil "~3D " (or (qnumber track) (1+ track-idx))))
                        (display-dur (format-runtime (runtime track)))
                        (display-name (truncate-string display-title title-w)))
                   (cursor-to row (+ x 2))
                   ;; Highlight selected row
                   (when selected-p
                     (inverse))
                   ;; Track number
                   (fg :track-num)
                   (when selected-p (inverse))
                   (princ display-num *terminal-io*)
                   ;; Track title
                   (if selected-p
                       (progn (fg :selected) (bold))
                       (fg :white))
                   (princ display-name *terminal-io*)
                   ;; Pad to duration column
                   (let ((pad (- title-w (length display-name))))
                     (when (> pad 0)
                       (princ (make-string pad :initial-element #\Space) *terminal-io*)))
                   ;; Duration
                   (fg :duration)
                   (when selected-p (inverse))
                   (format *terminal-io* " ~A" display-dur)
                   (reset)))))
    ;; Show empty state
    (unless tracks
      (cursor-to (+ y 2) (+ x 3))
      (fg :bright-black)
      (princ "(empty playlist)" *terminal-io*)
      (reset))
    (setf (panel-dirty-p panel) nil)))

;;; ── Track details panel ───────────────────────────────────────────

(defclass details-panel (panel)
  ((track :initarg :track :accessor details-panel-track :initform nil))
  (:documentation "Panel showing details of the currently selected track"))

(defun render-detail-row (row x w label value)
  "Render a labeled detail row inside the panel."
  (cursor-to row (+ x 2))
  (fg :bright-black)
  (bold)
  (princ label *terminal-io*)
  (reset)
  (princ " " *terminal-io*)
  (fg :white)
  (let* ((val-w (- w 4 (length label) 1))
         (display (if (and value (> (length value) val-w))
                      (truncate-string value val-w)
                      (or value "—"))))
    (princ display *terminal-io*))
  (reset))

(defmethod panel-render ((panel details-panel))
  (let* ((x (panel-x panel))
         (y (panel-y panel))
         (w (panel-width panel))
         (h (panel-height panel))
         (track (details-panel-track panel))
         (title (or (panel-title panel) "Details")))
    ;; Draw border
    (draw-box x y w h :title title :active-p (panel-active-p panel))
    ;; Clear content
    (clear-panel-content x y w h)
    (if track
        (let ((row (+ y 1)))
          (render-detail-row row x w "Title:"
                             (or (title track) "Unknown"))
          (render-detail-row (+ row 1) x w "Artist:"
                             (or (artist track) "Unknown"))
          (render-detail-row (+ row 2) x w "Path:"
                             (or (track-path track) "—"))
          (render-detail-row (+ row 3) x w "Duration:"
                             (format-runtime (runtime track)))
          (render-detail-row (+ row 4) x w "Position:"
                             (if (qnumber track)
                                 (format nil "#~D" (qnumber track))
                                 "—")))
        (progn
          (cursor-to (+ y 2) (+ x 3))
          (fg :bright-black)
          (princ "(no track selected)" *terminal-io*)
          (reset)))
    (setf (panel-dirty-p panel) nil)))

;;; ── File browser panel ────────────────────────────────────────────

(defparameter *audio-extensions*
  '("mp3" "flac")
  "Audio file extensions shown in the file browser.")

(defparameter *music-library-path*
  nil
  "Default path for the music library browser. Set via configuration or command line.")

(defstruct browser-entry
  "An entry in the file browser listing."
  (name "" :type string)
  (path "" :type string)
  (dir-p nil :type boolean)
  (selected-p nil :type boolean))

(defclass file-browser-panel (panel)
  ((current-dir   :initarg :current-dir   :accessor browser-current-dir
                  :initform (user-homedir-pathname)
                  :documentation "Directory currently being browsed")
   (entries       :initarg :entries       :accessor browser-entries
                  :initform nil
                  :documentation "List of browser-entry structs")
   (cursor        :initarg :cursor        :accessor browser-cursor
                  :initform 0)
   (scroll-offset :initarg :scroll-offset :accessor browser-scroll-offset
                  :initform 0)
   (recursive-p   :initarg :recursive-p   :accessor browser-recursive-p
                  :initform nil
                  :documentation "If T, show all audio files recursively"))
  (:documentation "File browser panel for navigating directories and selecting audio files"))

(defun browser-visible-height (panel)
  "Number of rows visible inside the browser border."
  (- (panel-height panel) 2))

(defun audio-file-p (path)
  "Return T if PATH has a recognized audio file extension."
  (let ((ext (pathname-type path)))
    (and ext (member ext *audio-extensions* :test #'string-equal))))

(defun collect-audio-files-recursive (dir)
  "Recursively collect all audio files under DIR."
  (let ((files nil))
    (handler-case
        (uiop:collect-sub*directories
         dir
         (constantly t)  ; recurse into all directories
         (constantly t)  ; collect all directories
         (lambda (subdir)
           (handler-case
               (dolist (f (uiop:directory-files subdir))
                 (when (audio-file-p f)
                   (push f files)))
             (error () nil))))
      (error () nil))
    (sort files #'string< :key #'namestring)))

(defun browser-refresh (panel)
  "Scan current-dir and populate entries with directories and audio files.
   If recursive-p is set, show all audio files recursively (no directories)."
  (let* ((dir (browser-current-dir panel))
         (entries nil))
    (if (browser-recursive-p panel)
        ;; Recursive mode: show all audio files under dir
        (let ((files (collect-audio-files-recursive dir)))
          (dolist (f files)
            (push (make-browser-entry
                   :name (enough-namestring f dir)
                   :path (namestring f)
                   :dir-p nil
                   :selected-p nil)
                  entries)))
        ;; Normal mode: show directories and audio files in current dir
        (progn
          ;; Parent directory entry (unless at root)
          (let ((parent (uiop:pathname-parent-directory-pathname dir)))
            (when (and parent (not (equal parent dir)))
              (push (make-browser-entry :name "../"
                                        :path (namestring parent)
                                        :dir-p t
                                        :selected-p nil)
                    entries)))
          ;; Subdirectories (sorted)
          (handler-case
              (let ((subdirs (sort (uiop:subdirectories dir)
                                   #'string< :key #'namestring)))
                (dolist (sub subdirs)
                  (let ((name (first (last (pathname-directory sub)))))
                    (when (and name (stringp name)
                               (not (string= name "."))
                               (not (string= name "..")))
                      (push (make-browser-entry
                             :name (concatenate 'string name "/")
                             :path (namestring sub)
                             :dir-p t
                             :selected-p nil)
                            entries)))))
            (error () nil))
          ;; Audio files (sorted)
          (handler-case
              (let ((files (sort (uiop:directory-files dir)
                                 #'string< :key #'namestring)))
                (dolist (f files)
                  (when (audio-file-p f)
                    (push (make-browser-entry
                           :name (file-namestring f)
                           :path (namestring f)
                           :dir-p nil
                           :selected-p nil)
                          entries))))
            (error () nil))))
    (setf (browser-entries panel) (nreverse entries)
          (browser-cursor panel) 0
          (browser-scroll-offset panel) 0)))

(defun browser-toggle-selection (panel)
  "Toggle selection on the current entry (audio files only)."
  (let ((entry (browser-selected-entry panel)))
    (when (and entry (not (browser-entry-dir-p entry)))
      (setf (browser-entry-selected-p entry)
            (not (browser-entry-selected-p entry))))))

(defun browser-selected-files (panel)
  "Return list of all selected browser-entry structs."
  (remove-if-not #'browser-entry-selected-p (browser-entries panel)))

(defun browser-selection-count (panel)
  "Return count of selected files."
  (count-if #'browser-entry-selected-p (browser-entries panel)))

(defun browser-selected-entry (panel)
  "Return the currently selected browser-entry, or NIL."
  (let ((entries (browser-entries panel))
        (idx (browser-cursor panel)))
    (when (and entries (< idx (length entries)))
      (nth idx entries))))

(defmethod panel-render ((panel file-browser-panel))
  (let* ((x (panel-x panel))
         (y (panel-y panel))
         (w (panel-width panel))
         (h (panel-height panel))
         (entries (browser-entries panel))
         (cursor (browser-cursor panel))
         (offset (browser-scroll-offset panel))
         (vis-h (browser-visible-height panel))
         (dir-str (namestring (browser-current-dir panel)))
         (sel-count (browser-selection-count panel))
         (title (if (> sel-count 0)
                    (format nil "~A [~D selected]"
                            (truncate-string dir-str (- w 20))
                            sel-count)
                    (truncate-string dir-str (- w 6)))))
    ;; Border
    (draw-box x y w h :title title :active-p t)
    (clear-panel-content x y w h)
    (if entries
        (let ((content-w (- w 5)))  ; leave room for selection marker
          (loop for i from 0 below vis-h
                for ei = (+ offset i)
                while (< ei (length entries))
                do (let* ((entry (nth ei entries))
                          (row (+ y 1 i))
                          (at-cursor-p (= ei cursor))
                          (name (browser-entry-name entry))
                          (is-dir (browser-entry-dir-p entry))
                          (marked-p (browser-entry-selected-p entry))
                          (display (truncate-string name content-w)))
                     (cursor-to row (+ x 2))
                     ;; Selection marker
                     (if marked-p
                         (progn (fg :green) (bold) (princ "✓" *terminal-io*) (reset))
                         (princ " " *terminal-io*))
                     (when at-cursor-p (inverse))
                     (if is-dir
                         (progn (fg :cyan) (when at-cursor-p (bold)))
                         (if marked-p
                             (fg :green)
                             (fg :white)))
                     (princ display *terminal-io*)
                     ;; Pad remainder
                     (let ((pad (- content-w (length display))))
                       (when (> pad 0)
                         (princ (make-string pad :initial-element #\Space)
                                *terminal-io*)))
                     (reset))))
        (progn
          (cursor-to (+ y 2) (+ x 3))
          (fg :bright-black)
          (princ "(empty directory)" *terminal-io*)
          (reset)))
    (setf (panel-dirty-p panel) nil)))
