;;; src/mcclim-app.lisp - McCLIM-based TUI for playlisp
;;; Uses charmed-mcclim backend for terminal rendering
;;;
;;; Usage:
;;;   (ql:quickload :playlisp)
;;;   (playlisp/src/mcclim-app:run)
;;;   (playlisp/src/mcclim-app:run "/path/to/playlist.m3u")

(uiop:define-package #:playlisp/src/mcclim-app
  (:use #:clim #:clim-lisp)
  (:import-from #:playlisp/parser
                #:playlist
                #:playlist-name
                #:playlist-elements
                #:track
                #:track-path
                #:title
                #:artist
                #:runtime
                #:parse-m3u-file
                #:make-playlist
                #:make-track)
  (:import-from #:playlisp/m3u-operations
                #:move-track-up
                #:move-track-down
                #:delete-track
                #:add-track
                #:write-m3u-file
                #:get-audio-duration
                #:make-track-from-file)
  (:export #:run
           #:*default-music-path*))

(in-package #:playlisp/src/mcclim-app)

;;; ── Configuration ────────────────────────────────────────────────────

(defparameter *default-music-path* 
  (merge-pathnames "Music/" (user-homedir-pathname))
  "Default path for browsing music files.")

(defparameter *audio-extensions* 
  '("mp3" "flac" "ogg" "opus" "m4a" "wav" "aac" "wma")
  "Recognized audio file extensions.")

;;; ── Browser State ───────────────────────────────────────────────────

(defstruct browser-entry
  "An entry in the file browser listing."
  (name "" :type string)
  (path "" :type string)
  (dir-p nil :type boolean)
  (selected-p nil :type boolean))

(defun audio-file-p (path)
  "Return T if PATH has a recognized audio file extension."
  (let ((ext (pathname-type path)))
    (and ext (member ext *audio-extensions* :test #'string-equal))))

(defun browser-refresh-entries (dir)
  "Scan DIR and return a list of browser-entry structs (dirs + audio files)."
  (let ((entries nil))
    ;; Parent directory
    (let ((parent (uiop:pathname-parent-directory-pathname dir)))
      (when (and parent (not (equal parent dir)))
        (push (make-browser-entry :name "../"
                                  :path (namestring parent)
                                  :dir-p t)
              entries)))
    ;; Subdirectories (sorted)
    (handler-case
        (let ((subdirs (sort (uiop:subdirectories dir)
                             #'string< :key #'namestring)))
          (dolist (sub subdirs)
            (let ((name (first (last (pathname-directory sub)))))
              (when (and name (stringp name))
                (push (make-browser-entry
                       :name (concatenate 'string name "/")
                       :path (namestring sub)
                       :dir-p t)
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
                     :dir-p nil)
                    entries))))
      (error () nil))
    (nreverse entries)))

;;; ── Presentation Types ───────────────────────────────────────────────

(define-presentation-type track-presentation ())
(define-presentation-type playlist-presentation ())
(define-presentation-type track-index ())

;;; ── Application Frame ────────────────────────────────────────────────

(define-application-frame playlisp-editor ()
  ((playlist :initform nil :accessor frame-playlist)
   (filepath :initform nil :accessor frame-filepath)
   (selected-index :initform 0 :accessor frame-selected-index)
   (message :initform nil :accessor frame-message)
   ;; Browser state
   (browser-dir :initform nil :accessor frame-browser-dir)
   (browser-entries :initform nil :accessor frame-browser-entries)
   (browser-cursor :initform 0 :accessor frame-browser-cursor)
   (browser-scroll :initform 0 :accessor frame-browser-scroll)
   (browse-mode-p :initform nil :accessor frame-browse-mode-p))
  (:panes
   (tracklist :application
              :scroll-bars nil
              :display-function 'display-tracklist
              :incremental-redisplay nil)
   (details :application
            :scroll-bars nil
            :display-function 'display-details
            :incremental-redisplay nil)
   (interactor :interactor
               :scroll-bars nil))
  (:layouts
   (default
    (vertically ()
      (1/2 tracklist)
      (1/4 details)
      (1/4 interactor))))
  (:top-level (default-frame-top-level))
  (:command-table (playlisp-editor
                   :inherit-from ())))

(defun print-prompt (stream frame)
  (declare (ignore frame))
  (with-drawing-options (stream :ink +cyan+)
    (format stream "playlisp> ")))

;;; Tell charmed-mcclim to pass arrow keys through when in browse mode
(defmethod clim-charmed:charmed-frame-wants-raw-keys-p ((frame playlisp-editor))
  (frame-browse-mode-p frame))

;;; ── Helpers ──────────────────────────────────────────────────────────

(defun frame-tracks (frame)
  "Return the list of tracks from the frame's playlist."
  (when (frame-playlist frame)
    (playlist-elements (frame-playlist frame))))

(defun frame-track-count (frame)
  (length (frame-tracks frame)))

(defun frame-selected-track (frame)
  "Return the currently selected track."
  (let ((tracks (frame-tracks frame))
        (idx (frame-selected-index frame)))
    (when (and tracks (< idx (length tracks)))
      (nth idx tracks))))

(defun format-duration (seconds)
  "Format duration in seconds as MM:SS or HH:MM:SS."
  (if (null seconds)
      "--:--"
      (let* ((s (round seconds))
             (h (floor s 3600))
             (m (floor (mod s 3600) 60))
             (sec (mod s 60)))
        (if (> h 0)
            (format nil "~D:~2,'0D:~2,'0D" h m sec)
            (format nil "~D:~2,'0D" m sec)))))

(defun truncate-string (str max-len)
  "Truncate STR to MAX-LEN, adding ellipsis if needed."
  (if (or (null str) (<= (length str) max-len))
      (or str "")
      (concatenate 'string (subseq str 0 (- max-len 1)) "…")))

;;; ── Display Functions ────────────────────────────────────────────────

(defun display-tracklist (frame pane)
  "Display the playlist tracks with the selected one highlighted."
  (let* ((playlist (frame-playlist frame))
         (tracks (frame-tracks frame))
         (selected (frame-selected-index frame))
         (name (if playlist (playlist-name playlist) "No Playlist")))
    ;; Header
    (with-text-face (pane :bold)
      (with-drawing-options (pane :ink +white+)
        (format pane " ♫ ~A" (truncate-string name 60))))
    (when (frame-filepath frame)
      (with-drawing-options (pane :ink +gray50+)
        (format pane " [~A]" (file-namestring (frame-filepath frame)))))
    (terpri pane)
    (let ((total-secs (loop for track in tracks
                            when (runtime track) sum (runtime track))))
      (with-drawing-options (pane :ink +gray50+)
        (format pane " ~D tracks  ~A" (length tracks) (format-duration total-secs))))
    (when (frame-message frame)
      (format pane "  ")
      (with-drawing-options (pane :ink +yellow+)
        (format pane "~A" (frame-message frame)))
      (setf (frame-message frame) nil))
    (terpri pane)
    (format pane "─────────────────────────────────────────────────────────────~%")
    ;; Track list
    (if (null tracks)
        (progn
          (terpri pane)
          (with-drawing-options (pane :ink +gray50+)
            (format pane "  (empty playlist)~%")
            (format pane "  Type 'Add' to add tracks or 'Open' to load a playlist~%")))
        (loop for track in tracks
              for i from 0
              for is-selected = (= i selected)
              do (display-track-line pane track i is-selected)))))

(defun display-track-line (pane track index selected-p)
  "Display a single track line, clickable."
  (let* ((the-title (or (title track) 
                        (file-namestring (track-path track))
                        "Unknown"))
         (the-artist (or (artist track) ""))
         (the-duration (format-duration (runtime track)))
         (display-title (truncate-string the-title 40))
         (display-artist (truncate-string the-artist 20)))
    ;; Selection indicator
    (if selected-p
        (with-drawing-options (pane :ink +cyan+)
          (format pane " ▶ "))
        (format pane "   "))
    ;; Track number
    (with-drawing-options (pane :ink +gray50+)
      (format pane "~3D " (1+ index)))
    ;; Clickable track
    (with-output-as-presentation (pane index 'track-index :single-box t)
      (if selected-p
          (with-text-face (pane :bold)
            (with-drawing-options (pane :ink +white+)
              (format pane "~A" display-title)))
          (with-drawing-options (pane :ink +white+)
            (format pane "~A" display-title))))
    ;; Artist
    (when (plusp (length the-artist))
      (with-drawing-options (pane :ink +gray50+)
        (format pane " - ~A" display-artist)))
    ;; Duration (right-aligned conceptually)
    (with-drawing-options (pane :ink +gray50+)
      (format pane "  [~A]" the-duration))
    (terpri pane)))

(defun display-details (frame pane)
  "Display details of the currently selected track."
  (let ((track (frame-selected-track frame)))
    (with-text-face (pane :bold)
      (with-drawing-options (pane :ink +cyan+)
        (format pane " Track Details~%")))
    (format pane "─────────────────────────────────────────────────────────────~%")
    (if (null track)
        (with-drawing-options (pane :ink +gray50+)
          (format pane "  No track selected~%"))
        (progn
          (format-detail-line pane "Title" (title track))
          (format-detail-line pane "Artist" (artist track))
          (format-detail-line pane "Duration" (format-duration (runtime track)))
          (format-detail-line pane "Path" (track-path track))))))

(defun format-detail-line (pane label value)
  "Format a label: value line in the details pane."
  (with-drawing-options (pane :ink +gray50+)
    (format pane "  ~12A: " label))
  (with-drawing-options (pane :ink +white+)
    (format pane "~A~%" (or value "-"))))

;;; ── Browser Helpers ────────────────────────────────────────────────

(defun enter-browse-mode (frame dir)
  "Switch to browse mode showing DIR."
  (let ((resolved (handler-case (truename dir) (error () dir))))
    (setf (frame-browser-dir frame) resolved
          (frame-browser-entries frame) (browser-refresh-entries resolved)
          (frame-browser-cursor frame) 0
          (frame-browser-scroll frame) 0
          (frame-browse-mode-p frame) t)
    (render-browser-to-interactor frame)))

(defun exit-browse-mode (frame)
  "Return to normal command mode."
  (setf (frame-browse-mode-p frame) nil))

(defun render-browser-to-interactor (frame)
  "Write the browser listing to the interactor pane."
  (let* ((stream (get-frame-pane frame 'interactor))
         (dir (frame-browser-dir frame))
         (entries (frame-browser-entries frame))
         (cursor (frame-browser-cursor frame)))
    (when (and stream dir)
      (window-clear stream)
      ;; Header
      (with-text-face (stream :bold)
        (with-drawing-options (stream :ink +cyan+)
          (format stream "~A~%" (namestring dir))))
      (with-drawing-options (stream :ink +gray50+)
        (format stream "──────────────────────────────────────────────────~%"))
      (with-drawing-options (stream :ink +gray50+)
        (format stream " Up/Down:nav  Enter:open  Space:add  Bksp:up  Esc:close~%"))
      ;; Entries
      (if (null entries)
          (with-drawing-options (stream :ink +gray50+)
            (format stream "  (empty directory)~%"))
          (loop for i from 0
                for entry in entries
                do (let ((is-cursor (= i cursor))
                         (name (browser-entry-name entry))
                         (is-dir (browser-entry-dir-p entry))
                         (is-selected (browser-entry-selected-p entry)))
                     (if is-cursor
                         (with-drawing-options (stream :ink +cyan+)
                           (format stream " > "))
                         (format stream "   "))
                     (if is-selected
                         (with-drawing-options (stream :ink +yellow+)
                           (format stream "* "))
                         (format stream "  "))
                     (if is-dir
                         (with-drawing-options (stream :ink +cyan+)
                           (format stream "~A~%" name))
                         (with-drawing-options (stream :ink +green+)
                           (format stream "~A~%" name))))))
      ;; Summary
      (let ((ndirs (count-if #'browser-entry-dir-p entries))
            (nfiles (count-if-not #'browser-entry-dir-p entries))
            (nsel (count-if #'browser-entry-selected-p entries)))
        (terpri stream)
        (with-drawing-options (stream :ink +gray50+)
          (format stream " ~D dirs, ~D audio files" ndirs nfiles)
          (when (> nsel 0)
            (with-drawing-options (stream :ink +yellow+)
              (format stream "  (~D selected)" nsel)))))
      (force-output stream))))

(defun browser-current-entry (frame)
  "Return the browser-entry under the cursor, or NIL."
  (let ((entries (frame-browser-entries frame))
        (idx (frame-browser-cursor frame)))
    (when (and entries (< idx (length entries)))
      (nth idx entries))))

;;; ── Commands ─────────────────────────────────────────────────────────

;; Navigation
(define-playlisp-editor-command (com-next-track :name "Next" :keystroke (#\n :control))
    ()
  (let* ((frame *application-frame*)
         (count (frame-track-count frame)))
    (when (< (frame-selected-index frame) (1- count))
      (incf (frame-selected-index frame))
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-prev-track :name "Previous" :keystroke (#\p :control))
    ()
  (let ((frame *application-frame*))
    (when (> (frame-selected-index frame) 0)
      (decf (frame-selected-index frame))
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-first-track :name "First" :keystroke (#\a :control))
    ()
  (let ((frame *application-frame*))
    (setf (frame-selected-index frame) 0)
    (redisplay-frame-panes frame)))

(define-playlisp-editor-command (com-last-track :name "Last" :keystroke (#\e :control))
    ()
  (let* ((frame *application-frame*)
         (count (frame-track-count frame)))
    (when (> count 0)
      (setf (frame-selected-index frame) (1- count))
      (redisplay-frame-panes frame))))

;; Track operations
(define-playlisp-editor-command (com-move-up :name "Move Up" :keystroke (#\u :control))
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame)))
    (when (and playlist (> idx 0))
      (move-track-up playlist idx)
      (decf (frame-selected-index frame))
      (setf (frame-message frame) "Track moved up")
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-move-down :name "Move Down" :keystroke (#\d :control))
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame))
         (count (frame-track-count frame)))
    (when (and playlist (< idx (1- count)))
      (move-track-down playlist idx)
      (incf (frame-selected-index frame))
      (setf (frame-message frame) "Track moved down")
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-delete-track :name "Delete" :keystroke (#\x :control))
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame))
         (count (frame-track-count frame)))
    (when (and playlist (> count 0))
      (delete-track playlist idx)
      ;; Adjust cursor if needed
      (when (>= (frame-selected-index frame) (1- count))
        (setf (frame-selected-index frame) (max 0 (- count 2))))
      (setf (frame-message frame) "Track deleted")
      (redisplay-frame-panes frame))))

;; File operations
(define-playlisp-editor-command (com-save :name "Save" :keystroke (#\s :control))
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (filepath (frame-filepath frame)))
    (cond
      ((null playlist)
       (setf (frame-message frame) "No playlist to save"))
      ((null filepath)
       (setf (frame-message frame) "No file path - use Save As"))
      (t
       (handler-case
           (progn
             (write-m3u-file playlist filepath)
             (setf (frame-message frame) 
                   (format nil "Saved to ~A" (file-namestring filepath))))
         (error (e)
           (setf (frame-message frame) 
                 (format nil "Save error: ~A" e))))))
    (redisplay-frame-panes frame)))

(define-playlisp-editor-command (com-open :name "Open")
    ((path 'pathname :prompt "M3U file"))
  (let ((frame *application-frame*))
    (handler-case
        (let ((playlist (parse-m3u-file path)))
          (setf (frame-playlist frame) playlist
                (frame-filepath frame) (namestring (truename path))
                (frame-selected-index frame) 0
                (frame-message frame) 
                (format nil "Loaded ~A" (file-namestring path))))
      (error (e)
        (setf (frame-message frame) 
              (format nil "Load error: ~A" e))))
    (redisplay-frame-panes frame)))

(define-playlisp-editor-command (com-new :name "New")
    ()
  (let ((frame *application-frame*))
    (setf (frame-playlist frame) (make-playlist "Untitled")
          (frame-filepath frame) nil
          (frame-selected-index frame) 0
          (frame-message frame) "New playlist created")
    (redisplay-frame-panes frame)))

;; Select track by clicking
(define-playlisp-editor-command (com-select-track :name "Select Track")
    ((index 'track-index))
  (let ((frame *application-frame*))
    (setf (frame-selected-index frame) index)
    (redisplay-frame-panes frame)))

;; Translator: clicking a track selects it
(define-presentation-to-command-translator click-track
    (track-index com-select-track playlisp-editor
     :gesture :select
     :documentation "Select this track")
    (object)
  (list object))

;; Add track from file (uses ffprobe for duration)
(define-playlisp-editor-command (com-add-track :name "Add")
    ((path 'pathname :prompt "Audio file"))
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame)))
    (when playlist
      (let ((track (make-track-from-file (namestring path))))
        (add-track playlist track (1+ (frame-selected-index frame)))
        (incf (frame-selected-index frame))
        (setf (frame-message frame) 
              (format nil "Added: ~A" (file-namestring path)))
        (redisplay-frame-panes frame)))))

;; Browse: enter interactive browser mode
(define-playlisp-editor-command (com-browse :name "Browse")
    ((dir 'pathname :prompt "Directory"))
  (enter-browse-mode *application-frame* dir))

;; Browser navigation commands (active in browse mode)
(define-playlisp-editor-command (com-browser-down :name "Nav Down")
    ()
  (let* ((frame *application-frame*)
         (entries (frame-browser-entries frame))
         (cursor (frame-browser-cursor frame)))
    (when (and (frame-browse-mode-p frame)
               entries
               (< cursor (1- (length entries))))
      (incf (frame-browser-cursor frame))
      (render-browser-to-interactor frame))))

(define-playlisp-editor-command (com-browser-up :name "Nav Up")
    ()
  (let ((frame *application-frame*))
    (when (and (frame-browse-mode-p frame)
               (> (frame-browser-cursor frame) 0))
      (decf (frame-browser-cursor frame))
      (render-browser-to-interactor frame))))

(define-playlisp-editor-command (com-browser-enter :name "Nav Enter")
    ()
  (let* ((frame *application-frame*)
         (entry (browser-current-entry frame)))
    (when (and (frame-browse-mode-p frame) entry)
      (if (browser-entry-dir-p entry)
          ;; Navigate into directory
          (let ((new-dir (pathname (browser-entry-path entry))))
            (setf (frame-browser-dir frame) new-dir
                  (frame-browser-entries frame) (browser-refresh-entries new-dir)
                  (frame-browser-cursor frame) 0
                  (frame-browser-scroll frame) 0))
          ;; Add audio file to playlist
          (let* ((path (browser-entry-path entry))
                 (name (browser-entry-name entry))
                 (playlist (frame-playlist frame)))
            (when playlist
              (let ((track (make-track-from-file path)))
                (add-track playlist track (1+ (frame-selected-index frame)))
                (incf (frame-selected-index frame))
                (setf (frame-message frame)
                      (format nil "Added: ~A" name))))))
      (redisplay-frame-panes frame)
      (render-browser-to-interactor frame))))

(define-playlisp-editor-command (com-browser-space :name "Nav Select")
    ()
  (let* ((frame *application-frame*)
         (entry (browser-current-entry frame)))
    (when (and (frame-browse-mode-p frame) entry)
      ;; Toggle selection on audio files
      (unless (browser-entry-dir-p entry)
        (setf (browser-entry-selected-p entry)
              (not (browser-entry-selected-p entry))))
      ;; Move to next entry
      (when (< (frame-browser-cursor frame)
               (1- (length (frame-browser-entries frame))))
        (incf (frame-browser-cursor frame)))
      (render-browser-to-interactor frame))))

(define-playlisp-editor-command (com-browser-add-selected :name "Nav Add All")
    ()
  (let* ((frame *application-frame*)
         (entries (frame-browser-entries frame))
         (selected (remove-if-not #'browser-entry-selected-p entries))
         (playlist (frame-playlist frame))
         (count 0))
    (when (and (frame-browse-mode-p frame) playlist selected)
      (dolist (entry selected)
        (let ((track (make-track-from-file (browser-entry-path entry))))
          (add-track playlist track (+ (frame-selected-index frame) count 1))
          (incf count)))
      (incf (frame-selected-index frame) count)
      ;; Clear selections
      (dolist (e entries)
        (setf (browser-entry-selected-p e) nil))
      (setf (frame-message frame)
            (format nil "Added ~D tracks" count))
      (redisplay-frame-panes frame)
      (render-browser-to-interactor frame))))

(define-playlisp-editor-command (com-browser-back :name "Nav Back")
    ()
  (let* ((frame *application-frame*)
         (dir (frame-browser-dir frame)))
    (when (and (frame-browse-mode-p frame) dir)
      (let ((parent (uiop:pathname-parent-directory-pathname dir)))
        (when (and parent (not (equal parent dir)))
          (setf (frame-browser-dir frame) parent
                (frame-browser-entries frame) (browser-refresh-entries parent)
                (frame-browser-cursor frame) 0
                (frame-browser-scroll frame) 0)
          (render-browser-to-interactor frame))))))

(define-playlisp-editor-command (com-browser-close :name "Nav Close")
    ()
  (when (frame-browse-mode-p *application-frame*)
    (exit-browse-mode *application-frame*)))

;; Refresh duration for selected track using ffprobe
(define-playlisp-editor-command (com-refresh-duration :name "Refresh Duration" :keystroke (#\r :control))
    ()
  (let* ((frame *application-frame*)
         (track (frame-selected-track frame)))
    (when track
      (let ((duration (get-audio-duration (track-path track))))
        (when duration
          (setf (runtime track) duration)
          (setf (frame-message frame) 
                (format nil "Duration: ~A" (format-duration duration)))
          (redisplay-frame-panes frame))))))

;; Help
(define-playlisp-editor-command (com-help :name "Help")
    ()
  (let ((stream (frame-standard-output *application-frame*)))
    (fresh-line stream)
    (with-text-face (stream :bold)
      (with-drawing-options (stream :ink +cyan+)
        (format stream "~%Playlisp Commands:~%")))
    (format stream "~%")
    (with-text-face (stream :bold)
      (format stream " Tracklist:~%"))
    (format stream "  Ctrl-N / Ctrl-P  - Next / Previous track~%")
    (format stream "  Ctrl-A / Ctrl-E  - First / Last track~%")
    (format stream "  Ctrl-U / Ctrl-D  - Move track up / down~%")
    (format stream "  Ctrl-X           - Delete selected track~%")
    (format stream "  Ctrl-R           - Refresh duration (ffprobe)~%")
    (format stream "~%")
    (with-text-face (stream :bold)
      (format stream " File Browser (after 'Browse /path/'):~%"))
    (format stream "  ↑/↓              - Navigate entries~%")
    (format stream "  Enter            - Open directory / add file~%")
    (format stream "  Space            - Toggle selection~%")
    (format stream "  Backspace        - Go to parent directory~%")
    (format stream "  Escape           - Close browser~%")
    (format stream "~%")
    (with-text-face (stream :bold)
      (format stream " General:~%"))
    (format stream "  Ctrl-S           - Save playlist~%")
    (format stream "  Ctrl-Q           - Quit~%")
    (format stream "~%")
    (format stream " Type: Open, Add, Browse, Save, New, Help, Quit~%")))

;; Quit
(define-playlisp-editor-command (com-quit :name "Quit" :keystroke (:q :control))
    ()
  (frame-exit *application-frame*))

;; Exit SBCL completely
(define-playlisp-editor-command (com-exit :name "Exit")
    ()
  (frame-exit *application-frame*))

;;; ── Browse Mode Key Dispatch ────────────────────────────────────────

;;; In browse mode we read key events directly from the interactor pane's
;;; event queue (charmed routes key events to the focused pane, not the
;;; frame queue).  This bypasses the normal command-line reader.
(defmethod read-frame-command ((frame playlisp-editor) &key stream)
  (declare (ignore stream))
  (if (frame-browse-mode-p frame)
      ;; Browse mode: read events from the interactor pane's event queue
      (let ((interactor (get-frame-pane frame 'interactor)))
        (loop
          (let ((event (event-read interactor)))
            (when (typep event 'key-press-event)
              (let ((key-name (keyboard-event-key-name event))
                    (char (keyboard-event-character event))
                    (mods (event-modifier-state event)))
                (cond
                  ((eql key-name :down)      (return '(com-browser-down)))
                  ((eql key-name :up)        (return '(com-browser-up)))
                  ((eql key-name :newline)   (return '(com-browser-enter)))
                  ((eql key-name :return)    (return '(com-browser-enter)))
                  ((eql key-name :backspace) (return '(com-browser-back)))
                  ((eql key-name :escape)    (return '(com-browser-close)))
                  ((eql char #\Space)        (return '(com-browser-space)))
                  ;; Ctrl-Q quits even in browse mode
                  ((and (eql key-name :|q|)
                        (not (zerop (logand mods +control-key+))))
                   (return '(com-quit)))
                  ;; Ctrl-S saves even in browse mode
                  ((and (eql key-name :|s|)
                        (not (zerop (logand mods +control-key+))))
                   (return '(com-save)))))))))
      ;; Normal mode: use standard command reading
      (call-next-method)))

;;; ── Standard Output ──────────────────────────────────────────────────

(defmethod frame-standard-output ((frame playlisp-editor))
  (get-frame-pane frame 'interactor))

;;; ── Entry Point ──────────────────────────────────────────────────────

(defun run (&optional filepath)
  "Launch the playlisp McCLIM TUI editor.
   If FILEPATH is given, load the M3U playlist from that path."
  (let* ((port (make-instance 'clim-charmed::charmed-port
                              :server-path '(:charmed)))
         (fm (first (slot-value port 'climi::frame-managers)))
         (event-queue (make-instance 'climi::simple-queue :port port))
         (input-buffer (make-instance 'climi::simple-queue :port port)))
    (unwind-protect
         (let ((frame (make-application-frame 'playlisp-editor
                                              :frame-manager fm
                                              :frame-event-queue event-queue
                                              :frame-input-buffer input-buffer)))
           ;; Load playlist if given
           (when filepath
             (handler-case
                 (let ((playlist (parse-m3u-file filepath)))
                   (setf (frame-playlist frame) playlist
                         (frame-filepath frame) (namestring (truename filepath))))
               (error (e)
                 (setf (frame-message frame) 
                       (format nil "Load error: ~A" e)))))
           ;; Default to empty playlist
           (unless (frame-playlist frame)
             (setf (frame-playlist frame) (make-playlist "Untitled")))
           ;; Run
           (run-frame-top-level frame))
      (climi::destroy-port port)))
  ;; Exit cleanly after frame closes
  (uiop:quit 0))
