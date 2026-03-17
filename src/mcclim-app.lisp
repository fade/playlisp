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
                #:write-m3u-file)
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

;;; ── Presentation Types ───────────────────────────────────────────────

(define-presentation-type track-presentation ())
(define-presentation-type playlist-presentation ())
(define-presentation-type track-index ())

;;; ── Application Frame ────────────────────────────────────────────────

(define-application-frame playlisp-editor ()
  ((playlist :initform nil :accessor frame-playlist)
   (filepath :initform nil :accessor frame-filepath)
   (selected-index :initform 0 :accessor frame-selected-index)
   (message :initform nil :accessor frame-message))
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
  (:top-level (default-frame-top-level :prompt 'print-prompt))
  (:command-table (playlisp-editor
                   :inherit-from ())))

(defun print-prompt (stream frame)
  (declare (ignore frame))
  (with-drawing-options (stream :ink +cyan+)
    (format stream "playlisp> ")))

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
    (with-drawing-options (pane :ink +gray50+)
      (format pane " ~D tracks" (length tracks)))
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
            (format pane "  Press 'a' to add tracks or 'o' to open a playlist~%")))
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

;;; ── Commands ─────────────────────────────────────────────────────────

;; Navigation
(define-playlisp-editor-command (com-next-track :name "Next" :keystroke (#\j))
    ()
  (let* ((frame *application-frame*)
         (count (frame-track-count frame)))
    (when (< (frame-selected-index frame) (1- count))
      (incf (frame-selected-index frame))
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-prev-track :name "Previous" :keystroke (#\k))
    ()
  (let ((frame *application-frame*))
    (when (> (frame-selected-index frame) 0)
      (decf (frame-selected-index frame))
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-first-track :name "First" :keystroke (#\g))
    ()
  (let ((frame *application-frame*))
    (setf (frame-selected-index frame) 0)
    (redisplay-frame-panes frame)))

(define-playlisp-editor-command (com-last-track :name "Last" :keystroke (#\G))
    ()
  (let* ((frame *application-frame*)
         (count (frame-track-count frame)))
    (when (> count 0)
      (setf (frame-selected-index frame) (1- count))
      (redisplay-frame-panes frame))))

;; Track operations
(define-playlisp-editor-command (com-move-up :name "Move Up" :keystroke (#\K))
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame)))
    (when (and playlist (> idx 0))
      (move-track-up playlist idx)
      (decf (frame-selected-index frame))
      (setf (frame-message frame) "Track moved up")
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-move-down :name "Move Down" :keystroke (#\J))
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

(define-playlisp-editor-command (com-delete-track :name "Delete" :keystroke (#\d))
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
(define-playlisp-editor-command (com-save :name "Save" :keystroke (#\w))
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

(define-playlisp-editor-command (com-new :name "New" :keystroke (#\n))
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

;; Add track from file
(define-playlisp-editor-command (com-add-track :name "Add" :keystroke (#\a))
    ((path 'pathname :prompt "Audio file"))
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame)))
    (when playlist
      (let ((track (make-track (pathname-name path) (namestring path))))
        (add-track playlist track (1+ (frame-selected-index frame)))
        (incf (frame-selected-index frame))
        (setf (frame-message frame) 
              (format nil "Added: ~A" (file-namestring path)))
        (redisplay-frame-panes frame)))))

;; Browse directory and show files
(define-playlisp-editor-command (com-browse :name "Browse" :keystroke (#\b))
    ((dir 'pathname :prompt "Directory" 
          :default *default-music-path*))
  (let ((stream (frame-standard-output *application-frame*)))
    (fresh-line stream)
    (with-text-face (stream :bold)
      (format stream "~%Files in ~A:~%" dir))
    (handler-case
        (let ((files (directory (merge-pathnames "*.*" dir))))
          (dolist (file files)
            (let ((name (file-namestring file))
                  (ext (pathname-type file)))
              (cond
                ((uiop:directory-pathname-p file)
                 (with-drawing-options (stream :ink +cyan+)
                   (format stream "  [DIR] ~A~%" (car (last (pathname-directory file))))))
                ((member ext *audio-extensions* :test #'string-equal)
                 (with-output-as-presentation (stream file 'pathname :single-box t)
                   (with-drawing-options (stream :ink +green+)
                     (format stream "  ♫ ~A~%" name))))
                (t
                 (with-drawing-options (stream :ink +gray50+)
                   (format stream "    ~A~%" name))))))
          (format stream "~%Click a file to add it, or type: Add /path/to/file~%"))
      (error (e)
        (format stream "Error: ~A~%" e)))))

;; Translator: clicking a pathname adds it as a track
(define-presentation-to-command-translator click-file-to-add
    (pathname com-add-track playlisp-editor
     :gesture :select
     :documentation "Add this file to playlist")
    (object)
  (list object))

;; Help
(define-playlisp-editor-command (com-help :name "Help" :keystroke (#\?))
    ()
  (let ((stream (frame-standard-output *application-frame*)))
    (fresh-line stream)
    (with-text-face (stream :bold)
      (format stream "~%Playlisp Keybindings:~%"))
    (format stream "  j/k     - Navigate tracks~%")
    (format stream "  J/K     - Move track down/up~%")
    (format stream "  g/G     - Go to first/last track~%")
    (format stream "  d       - Delete selected track~%")
    (format stream "  a       - Add track (prompts for file)~%")
    (format stream "  b       - Browse directory~%")
    (format stream "  n       - New playlist~%")
    (format stream "  w       - Save playlist~%")
    (format stream "  ?       - Show this help~%")
    (format stream "  Tab     - Switch pane focus~%")
    (format stream "  Ctrl-Q  - Quit~%")
    (format stream "~%Commands: Open, Add, Browse, Save, New, Help, Quit~%")))

;; Quit
(define-playlisp-editor-command (com-quit :name "Quit" :keystroke (:q :control))
    ()
  (frame-exit *application-frame*))

;; Exit SBCL completely
(define-playlisp-editor-command (com-exit :name "Exit")
    ()
  (frame-exit *application-frame*))

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
