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
                #:playlist-phase
                #:playlist-duration
                #:playlist-curator
                #:playlist-description
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
                #:add-playlist-element
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

(defun dir-string-to-directory-list (dir-string)
  "Split DIR-STRING on / into a pathname directory component list.
   This avoids parse-namestring which interprets [] as wildcards."
  (let* ((trimmed (string-right-trim "/" dir-string))
         (parts (uiop:split-string trimmed :separator "/"))
         (dirs (remove-if (lambda (s) (zerop (length s))) parts)))
    (cons :absolute dirs)))

(defun list-subdirectories (dir-string)
  "List subdirectories of DIR-STRING, handling special characters like [] in paths."
  (handler-case
      (let ((wild (make-pathname
                   :directory (append (dir-string-to-directory-list dir-string)
                                     (list :wild))
                   :name nil :type nil)))
        (sort (remove-if-not
               (lambda (p) (uiop:directory-pathname-p p))
               (directory wild))
              #'string< :key #'namestring))
    (error () nil)))

(defun list-directory-files (dir-string)
  "List files in DIR-STRING, handling special characters like [] in paths."
  (handler-case
      (let ((wild (make-pathname
                   :directory (dir-string-to-directory-list dir-string)
                   :name :wild :type :wild)))
        (sort (remove-if
               (lambda (p) (uiop:directory-pathname-p p))
               (directory wild))
              #'string< :key #'namestring))
    (error () nil)))

(defun browser-refresh-entries (dir)
  "Scan DIR and return a list of browser-entry structs (dirs + audio files).
   DIR may be a pathname or a string. Paths are built as strings to preserve
   symlink paths and avoid wildcard interpretation of brackets."
  (let* ((entries nil)
         (dir-str (if (stringp dir) dir (namestring dir)))
         ;; Ensure trailing slash
         (dir-str (if (eql (char dir-str (1- (length dir-str))) #\/)
                      dir-str
                      (concatenate 'string dir-str "/"))))
    ;; Parent directory
    (let* ((trimmed (string-right-trim "/" (subseq dir-str 0 (1- (length dir-str)))))
           (slash-pos (position #\/ trimmed :from-end t)))
      (when slash-pos
        (let ((parent (subseq dir-str 0 (1+ slash-pos))))
          (push (make-browser-entry :name "../"
                                    :path parent
                                    :dir-p t)
                entries))))
    ;; Subdirectories (sorted)
    (let ((subdirs (list-subdirectories dir-str)))
      (dolist (sub subdirs)
        (let ((name (first (last (pathname-directory sub)))))
          (when (and name (stringp name))
            (push (make-browser-entry
                   :name (concatenate 'string name "/")
                   :path (concatenate 'string dir-str name "/")
                   :dir-p t)
                  entries)))))
    ;; Audio files (sorted)
    (let ((files (list-directory-files dir-str)))
      (dolist (f files)
        (when (audio-file-p f)
          (push (make-browser-entry
                 :name (file-namestring f)
                 :path (concatenate 'string dir-str (file-namestring f))
                 :dir-p nil)
                entries))))
    (nreverse entries)))

;;; ── Presentation Types ───────────────────────────────────────────────

(define-presentation-type track-presentation ())
(define-presentation-type playlist-presentation ())
(define-presentation-type track-index ())

;;; ── Application Frame ────────────────────────────────────────────────

(define-application-frame playlisp-editor ()
  ((playlist :initform nil :accessor frame-playlist)
   (filepath :initarg :filepath :initform nil :accessor frame-filepath)
   (selected-index :initform 0 :accessor frame-selected-index)
   (message :initform nil :accessor frame-message)
   ;; Browser state
   (browser-root :initform nil :accessor frame-browser-root)
   (browser-dir :initform nil :accessor frame-browser-dir)
   (browser-entries :initform nil :accessor frame-browser-entries)
   (browser-cursor :initform 0 :accessor frame-browser-cursor)
   (browser-scroll :initform 0 :accessor frame-browser-scroll)
   (browse-mode-p :initform nil :accessor frame-browse-mode-p)
   (edit-mode-p :initform nil :accessor frame-edit-mode-p))
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
      (9/20 tracklist)
      (3/20 details)
      (2/5 interactor))))
  (:top-level (clim-charmed:charmed-frame-top-level :prompt 'print-prompt))
  (:command-table (playlisp-editor
                   :inherit-from ())))

(defun print-prompt (stream frame)
  (unless (or (frame-browse-mode-p frame)
              (frame-edit-mode-p frame))
    (with-drawing-options (stream :ink +cyan+)
      (format stream "playlisp> "))))

;;; Tell charmed-mcclim to pass arrow keys through when in browse mode
(defmethod clim-charmed:charmed-frame-wants-raw-keys-p ((frame playlisp-editor))
  (or (frame-browse-mode-p frame)
      (frame-edit-mode-p frame)))

;;; Tell charmed-mcclim which pane is active for border highlighting.
;;; Edit mode: tracklist (user navigates tracks in top pane).
;;; Browse mode + normal: interactor (user interacts in bottom pane).
(defmethod clim-charmed:charmed-active-pane ((frame playlisp-editor))
  (if (frame-edit-mode-p frame)
      (get-frame-pane frame 'tracklist)
      (get-frame-pane frame 'interactor)))

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
  (if (or (null seconds) (< seconds 0))
      "--:--"
      (let* ((s (round seconds))
             (h (floor s 3600))
             (m (floor (mod s 3600) 60))
             (sec (mod s 60)))
        (if (> h 0)
            (format nil "~D:~2,'0D:~2,'0D" h m sec)
            (format nil "~D:~2,'0D" m sec)))))

(defun format-human-duration (seconds)
  "Format total SECONDS as a human-readable string like '1 hour 26 minutes'."
  (if (or (null seconds) (<= seconds 0))
      nil
      (let* ((s (round seconds))
             (h (floor s 3600))
             (m (floor (mod s 3600) 60)))
        (cond
          ((and (> h 0) (> m 0))
           (format nil "~D hour~:P ~D minute~:P" h m))
          ((> h 0)
           (format nil "~D hour~:P" h))
          ((> m 0)
           (format nil "~D minute~:P" m))
          (t (format nil "~D seconds" s))))))

(defun compute-playlist-duration (playlist)
  "Sum all track runtimes in PLAYLIST and update its duration field."
  (let* ((tracks (playlist-elements playlist))
         (total (loop for track in tracks
                      when (and (runtime track) (plusp (runtime track)))
                      sum (runtime track))))
    (when (plusp total)
      (setf (playlist-duration playlist) (format-human-duration total)))))

(defun truncate-string (str max-len)
  "Truncate STR to MAX-LEN, adding ellipsis if needed."
  (if (or (null str) (<= (length str) max-len))
      (or str "")
      (concatenate 'string (subseq str 0 (- max-len 1)) "…")))

;;; ── Display Functions ────────────────────────────────────────────────

(defun get-pane-columns (frame pane)
  "Return the number of columns available in PANE, or a default."
  (let* ((fm (frame-manager frame))
         (port (when fm (port fm))))
    (if (and port (typep port 'clim-charmed::charmed-port))
        (let ((vp (gethash pane (clim-charmed::charmed-port-viewport-sizes port))))
          (if vp (max 40 (round (third vp))) 80))
        80)))

(defun get-pane-rows (frame pane)
  "Return the number of rows available in PANE, or a default."
  (let* ((fm (frame-manager frame))
         (port (when fm (port fm))))
    (if (and port (typep port 'clim-charmed::charmed-port))
        (let ((vp (gethash pane (clim-charmed::charmed-port-viewport-sizes port))))
          (if vp (max 10 (round (fourth vp))) 25))
        25)))

(defun frame-tracklist-active-p (frame)
  "Return T when the tracklist pane should be highlighted as active."
  (frame-edit-mode-p frame))

(defun display-tracklist (frame pane)
  "Display the playlist tracks with the selected one highlighted."
  ;; Reset charmed scroll offset so header stays at top
  (let* ((fm (frame-manager frame))
         (port (when fm (port fm))))
    (when (and port (typep port 'clim-charmed::charmed-port))
      (setf (clim-charmed::pane-scroll-offset port pane) 0)))
  (let* ((playlist (frame-playlist frame))
         (tracks (frame-tracks frame))
         (selected (frame-selected-index frame))
         (name (if playlist (playlist-name playlist) "No Playlist"))
         (cols (get-pane-columns frame pane))
         (total-rows (get-pane-rows frame pane))
         (header-rows 3)  ;; title line, info line, separator
         (track-rows (max 1 (- total-rows header-rows)))
         (num-tracks (length tracks))
         (active-p (frame-tracklist-active-p frame))
         (border-ink (if active-p +cyan+ +gray50+))
         (title-ink (if active-p +white+ +gray50+)))
    ;; Header
    (when (frame-edit-mode-p frame)
      (with-text-face (pane :bold)
        (with-drawing-options (pane :ink +yellow+)
          (format pane " [EDIT] "))))
    (when (frame-browse-mode-p frame)
      (with-text-face (pane :bold)
        (with-drawing-options (pane :ink +green+)
          (format pane " [BROWSE] "))))
    (with-text-face (pane :bold)
      (with-drawing-options (pane :ink title-ink)
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
    (with-drawing-options (pane :ink border-ink)
      (format pane "~A~%" (make-string (min cols 80) :initial-element #\─)))
    ;; Track list - paginated to fit pane
    (if (null tracks)
        (progn
          (terpri pane)
          (with-drawing-options (pane :ink +gray50+)
            (format pane "  (empty playlist)~%")
            (format pane "  Type 'Add' to add tracks or 'Open' to load a playlist~%")))
        (let* ((num-tracks (length tracks))
               ;; Selected track uses 2 rows (track + underline), others use 1
               (visible (max 1 (1- track-rows)))
               ;; Compute scroll window: keep selected track visible
               (scroll-start (max 0 (min (- num-tracks visible)
                                         (- selected (floor visible 2)))))
               (scroll-end (min num-tracks (+ scroll-start visible))))
          ;; Show scroll indicator at top if not at beginning
          (when (> scroll-start 0)
            (with-drawing-options (pane :ink +gray50+)
              (format pane "   ↑ ~D more~%" scroll-start)))
          ;; Render visible tracks
          (loop for i from scroll-start below scroll-end
                for track = (nth i tracks)
                for is-selected = (= i selected)
                do (display-track-line pane track i is-selected cols active-p))
          ;; Show scroll indicator at bottom if not at end
          (when (< scroll-end num-tracks)
            (with-drawing-options (pane :ink +gray50+)
              (format pane "   ↓ ~D more~%" (- num-tracks scroll-end))))))))

(defun display-track-line (pane track index selected-p &optional (cols 80) (active-p t))
  "Display a single track line, clickable."
  ;; Reserve space: 3 (indicator) + 4 (num) + 10 (duration+brackets) = ~17 overhead
  (let* ((title-width (max 20 (- cols 17)))
         (highlight-ink (if active-p +cyan+ +gray50+))
         (the-title (or (title track) 
                        (file-namestring (track-path track))
                        "Unknown"))
         (the-artist (or (artist track) ""))
         (the-duration (format-duration (runtime track)))
         (duration-str (format nil "  [~A]" the-duration))
         ;; Compute how much space title+artist can use
         (avail-for-text (max 20 (- cols 7 (length duration-str))))
         (display-title (truncate-string the-title avail-for-text)))
    ;; Selection indicator
    (if selected-p
        (with-drawing-options (pane :ink highlight-ink)
          (format pane " ▶ "))
        (format pane "   "))
    ;; Track number
    (with-drawing-options (pane :ink +gray50+)
      (format pane "~3D " (1+ index)))
    ;; Clickable track
    (with-output-as-presentation (pane index 'track-index :single-box t)
      (if selected-p
          (with-text-face (pane :bold)
            (with-drawing-options (pane :ink +white+
                                       :text-style (make-text-style nil nil nil))
              (format pane "~A" display-title)))
          (with-drawing-options (pane :ink +white+)
            (format pane "~A" display-title))))
    ;; Duration
    (with-drawing-options (pane :ink +gray50+)
      (format pane "~A" duration-str))
    (terpri pane)
    ;; Underline for selected track
    (when selected-p
      (with-drawing-options (pane :ink highlight-ink)
        (format pane "~A~%" (make-string (min cols 80) :initial-element #\─))))))


(defun display-details (frame pane)
  "Display details of the currently selected track."
  (let ((track (frame-selected-track frame)))
    (if (null track)
        (with-drawing-options (pane :ink +gray50+)
          (format pane "  No track selected~%"))
        (progn
          (format-detail-line pane "Title" (title track))
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
  (let* ((resolved (namestring (merge-pathnames dir)))
         (resolved (if (eql (char resolved (1- (length resolved))) #\/)
                       resolved
                       (concatenate 'string resolved "/"))))
    (setf (frame-browser-root frame) resolved
          (frame-browser-dir frame) resolved
          (frame-browser-entries frame) (browser-refresh-entries resolved)
          (frame-browser-cursor frame) 0
          (frame-browser-scroll frame) 0
          (frame-browse-mode-p frame) t)
    (render-browser-to-interactor frame)))

(defun exit-browse-mode (frame)
  "Return to normal command mode, clearing browser display."
  (setf (frame-browse-mode-p frame) nil)
  ;; Clear the interactor's charmed screen area and output history
  (let* ((stream (get-frame-pane frame 'interactor))
         (fm (frame-manager frame))
         (port (when fm (port fm))))
    (when (and stream port (typep port 'clim-charmed::charmed-port))
      (let ((screen (clim-charmed::charmed-port-screen port))
            (vp (gethash stream (clim-charmed::charmed-port-viewport-sizes port))))
        (when (and screen vp)
          (charmed:screen-fill-rect screen
                                    (round (first vp))
                                    (round (second vp))
                                    (round (third vp))
                                    (round (fourth vp)))))
      (setf (clim-charmed::pane-scroll-offset port stream) 0))
    (when stream
      (window-clear stream)))
  ;; Redisplay all panes to restore normal UI
  (redisplay-frame-panes frame))

(defun render-browser-to-interactor (frame)
  "Write the browser listing to the interactor pane."
  (let* ((stream (get-frame-pane frame 'interactor))
         (dir (frame-browser-dir frame))
         (entries (frame-browser-entries frame))
         (cursor (frame-browser-cursor frame))
         (fm (frame-manager frame))
         (port (when fm (port fm))))
    (when (and stream dir)
      ;; Clear the interactor's area on the charmed screen buffer
      ;; and reset scroll offset so content draws from the top
      (when (and port (typep port 'clim-charmed::charmed-port))
        (let ((screen (clim-charmed::charmed-port-screen port))
              (vp (gethash stream (clim-charmed::charmed-port-viewport-sizes port))))
          (when (and screen vp)
            (charmed:screen-fill-rect screen
                                      (round (first vp))
                                      (round (second vp))
                                      (round (third vp))
                                      (round (fourth vp)))))
        ;; Reset scroll offset to 0 so text draws from the top of the viewport
        (setf (clim-charmed::pane-scroll-offset port stream) 0))
      (window-clear stream)
      ;; Header
      (with-text-face (stream :bold)
        (with-drawing-options (stream :ink +cyan+)
          (format stream "~A~%" (namestring dir))))
      (with-drawing-options (stream :ink +gray50+)
        (format stream "──────────────────────────────────────────────────~%"))
      (with-drawing-options (stream :ink +gray50+)
        (format stream " ↑↓/C-n C-p:nav  Enter:open  Space:star  a:add  Bksp:up  Esc:close~%"))
      ;; Entries - show a scrolling window that keeps the cursor visible.
      ;; 3 header lines + 2 summary lines = 5 overhead lines.
      (let* ((vp (when (and port (typep port 'clim-charmed::charmed-port))
                   (gethash stream (clim-charmed::charmed-port-viewport-sizes port))))
             (viewport-lines (if vp (max 1 (- (round (fourth vp)) 5)) 20))
             (n (length entries))
             (scroll (frame-browser-scroll frame))
             ;; Adjust scroll to keep cursor visible
             (scroll (cond ((< cursor scroll) cursor)
                           ((>= cursor (+ scroll viewport-lines))
                            (- cursor viewport-lines -1))
                           (t scroll)))
             (end (min n (+ scroll viewport-lines))))
        (setf (frame-browser-scroll frame) scroll)
        (if (null entries)
            (with-drawing-options (stream :ink +gray50+)
              (format stream "  (empty directory)~%"))
            (progn
              ;; Show scroll-up indicator
              (when (> scroll 0)
                (with-drawing-options (stream :ink +gray50+)
                  (format stream "   ... ~D more above ...~%" scroll)))
              (loop for i from scroll below end
                    for entry = (nth i entries)
                    do (let ((is-cursor (= i cursor))
                             (name (browser-entry-name entry))
                             (is-dir (browser-entry-dir-p entry))
                             (is-selected (browser-entry-selected-p entry)))
                         (if is-cursor
                             (with-drawing-options (stream :ink +cyan+)
                               (format stream " ▶ "))
                             (format stream "   "))
                         (if is-selected
                             (with-drawing-options (stream :ink +yellow+)
                               (format stream "* "))
                             (format stream "  "))
                         (if is-dir
                             (with-drawing-options (stream :ink +cyan+)
                               (format stream "~A~%" name))
                             (with-drawing-options (stream :ink +green+)
                               (format stream "~A~%" name)))))
              ;; Show scroll-down indicator
              (when (< end n)
                (with-drawing-options (stream :ink +gray50+)
                  (format stream "   ... ~D more below ...~%" (- n end)))))))
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
      (force-output stream)
      ;; Force charmed port to present the screen.
      ;; We cannot use redisplay-frame-panes here because it calls
      ;; reset-stream-cursor on the interactor, clobbering our output.
      ;; Instead, directly call port-force-output which draws borders,
      ;; positions cursor, and calls screen-present.
      (let* ((fm (frame-manager frame))
             (port (when fm (port fm))))
        (when (and port (typep port 'clim-charmed::charmed-port))
          (climi::port-force-output port))))))

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

(define-playlisp-editor-command (com-last-track :name "Last")
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
      (delete-track idx playlist)
      ;; Adjust cursor if needed
      (when (>= (frame-selected-index frame) (1- count))
        (setf (frame-selected-index frame) (max 0 (- count 2))))
      (setf (frame-message frame) "Track deleted")
      (redisplay-frame-panes frame))))

;; Edit mode commands
(define-playlisp-editor-command (com-enter-edit-mode :name "Edit" :keystroke (#\e :control))
    ()
  (let ((frame *application-frame*))
    (when (and (frame-playlist frame)
               (plusp (frame-track-count frame)))
      (setf (frame-edit-mode-p frame) t
            (frame-message frame) "EDIT: ↑↓/C-n C-p nav  u/d move  x del  ESC exit")
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-edit-close :name "Exit Edit")
    ()
  (let* ((frame *application-frame*)
         (stream (get-frame-pane frame 'interactor))
         (fm (frame-manager frame))
         (port (when fm (port fm))))
    (setf (frame-edit-mode-p frame) nil
          (frame-message frame) "Edit mode exited")
    ;; Clear and reset the interactor so charmed restores keyboard input
    (when (and stream port (typep port 'clim-charmed::charmed-port))
      (let ((screen (clim-charmed::charmed-port-screen port))
            (vp (gethash stream (clim-charmed::charmed-port-viewport-sizes port))))
        (when (and screen vp)
          (charmed:screen-fill-rect screen
                                    (round (first vp))
                                    (round (second vp))
                                    (round (third vp))
                                    (round (fourth vp)))))
      (setf (clim-charmed::pane-scroll-offset port stream) 0))
    (when stream
      (window-clear stream))
    (redisplay-frame-panes frame)))

(define-playlisp-editor-command (com-edit-up :name "Edit Up")
    ()
  (let* ((frame *application-frame*)
         (idx (frame-selected-index frame)))
    (when (> idx 0)
      (decf (frame-selected-index frame))
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-edit-down :name "Edit Down")
    ()
  (let* ((frame *application-frame*)
         (idx (frame-selected-index frame))
         (count (frame-track-count frame)))
    (when (< idx (1- count))
      (incf (frame-selected-index frame))
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-edit-move-up :name "Edit Move Up")
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame)))
    (when (and playlist (> idx 0))
      (move-track-up playlist idx)
      (decf (frame-selected-index frame))
      (setf (frame-message frame) "Track moved up")
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-edit-move-down :name "Edit Move Down")
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

(define-playlisp-editor-command (com-edit-delete :name "Edit Delete")
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (idx (frame-selected-index frame))
         (count (frame-track-count frame)))
    (when (and playlist (> count 0))
      (delete-track idx playlist)
      (when (>= (frame-selected-index frame) (1- count))
        (setf (frame-selected-index frame) (max 0 (- count 2))))
      (setf (frame-message frame) "Track deleted")
      ;; Exit edit mode if playlist is now empty
      (when (zerop (frame-track-count frame))
        (setf (frame-edit-mode-p frame) nil
              (frame-message frame) "Track deleted - playlist empty, edit mode exited"))
      (redisplay-frame-panes frame))))

;; File operations

(defun prompt-playlist-metadata (frame playlist)
  "Prompt user for playlist metadata fields via the interactor pane.
   Only updates fields where the user provides non-empty input."
  (let ((stream (get-frame-pane frame 'interactor)))
    (when stream
      ;; Playlist name
      (let ((current (or (playlist-name playlist) "Untitled")))
        (format stream "~&Playlist name [~A]: " current)
        (finish-output stream)
        (let ((input (accept 'string :stream stream :prompt "" :default current)))
          (when (and input (plusp (length input)))
            (setf (playlist-name playlist) input))))
      ;; Phase
      (let ((current (or (playlist-phase playlist) "")))
        (format stream "~&Phase [~A]: " current)
        (finish-output stream)
        (let ((input (accept 'string :stream stream :prompt "" :default current)))
          (when (and input (plusp (length input)))
            (setf (playlist-phase playlist) input))))
      ;; Curator
      (let ((current (or (playlist-curator playlist)
                         (uiop:getenv "USER") "")))
        (format stream "~&Curator [~A]: " current)
        (finish-output stream)
        (let ((input (accept 'string :stream stream :prompt "" :default current)))
          (when (and input (plusp (length input)))
            (setf (playlist-curator playlist) input))))
      ;; Description
      (let ((current (or (playlist-description playlist) "")))
        (format stream "~&Description [~A]: " current)
        (finish-output stream)
        (let ((input (accept 'string :stream stream :prompt "" :default current)))
          (when (and input (plusp (length input)))
            (setf (playlist-description playlist) input)))))))

(defun normalize-save-path (path &optional existing-filepath)
  "Normalize PATH for saving: expand ~, ensure .m3u extension, default directory."
  (let* ((expanded (if (and (plusp (length path))
                            (char= (char path 0) #\~))
                       (concatenate 'string (namestring (user-homedir-pathname))
                                    (subseq path (if (and (> (length path) 1)
                                                          (char= (char path 1) #\/))
                                                     2 1)))
                       path))
         ;; If no directory separator, place in same dir as existing file or cwd
         (with-dir (if (position #\/ expanded)
                       expanded
                       (let ((dir (if existing-filepath
                                      (directory-namestring
                                       (pathname existing-filepath))
                                      (namestring (uiop:getcwd)))))
                         (concatenate 'string dir expanded))))
         ;; Ensure .m3u extension
         (with-ext (if (or (uiop:string-suffix-p with-dir ".m3u")
                           (uiop:string-suffix-p with-dir ".m3u8")
                           (uiop:string-suffix-p with-dir ".M3U"))
                       with-dir
                       (concatenate 'string with-dir ".m3u"))))
    with-ext))

(defun do-save-playlist (frame filepath)
  "Save the current playlist to FILEPATH, updating duration automatically."
  (let* ((playlist (frame-playlist frame))
         (filepath (normalize-save-path filepath (frame-filepath frame))))
    (when playlist
      ;; Auto-compute duration from track runtimes
      (compute-playlist-duration playlist)
      (handler-case
          (progn
            (write-m3u-file playlist filepath)
            (setf (frame-filepath frame) filepath
                  (frame-message frame) 
                  (format nil "Saved to ~A" filepath)))
        (error (e)
          (setf (frame-message frame) 
                (format nil "Save error: ~A" e))))
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-save :name "Save" :keystroke (#\s :control))
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (filepath (frame-filepath frame)))
    (cond
      ((null playlist)
       (setf (frame-message frame) "No playlist to save")
       (redisplay-frame-panes frame))
      ((null filepath)
       ;; New playlist - prompt for metadata and filepath
       (let ((stream (get-frame-pane frame 'interactor)))
         (prompt-playlist-metadata frame playlist)
         (format stream "~&Save as: ")
         (finish-output stream)
         (let ((path (accept 'string :stream stream :prompt "")))
           (when (and path (plusp (length path)))
             (do-save-playlist frame path)))))
      (t
       ;; Existing filepath - just save
       (do-save-playlist frame filepath)))))

(define-playlisp-editor-command (com-save-as :name "Save As")
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame)))
    (cond
      ((null playlist)
       (setf (frame-message frame) "No playlist to save")
       (redisplay-frame-panes frame))
      (t
       (let ((stream (get-frame-pane frame 'interactor)))
         (prompt-playlist-metadata frame playlist)
         (format stream "~&Save as: ")
         (finish-output stream)
         (let ((path (accept 'string :stream stream :prompt "")))
           (when (and path (plusp (length path)))
             (do-save-playlist frame path))))))))

(define-playlisp-editor-command (com-write :name "Write")
    ()
  (com-save))

(define-playlisp-editor-command (com-edit-metadata :name "Metadata")
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame)))
    (when playlist
      (prompt-playlist-metadata frame playlist)
      (setf (frame-message frame) "Metadata updated")
      (redisplay-frame-panes frame))))

(define-playlisp-editor-command (com-open :name "Open")
    ((path 'pathname :prompt "M3U file"))
  (let ((frame *application-frame*))
    (handler-case
        (let ((playlist (parse-m3u-file path)))
          (setf (frame-playlist frame) playlist
                (frame-filepath frame) (namestring (truename path))
                (frame-selected-index frame) 0
                (frame-message frame)
                (format nil "Loading ~A - probing durations..." (file-namestring path)))
          (redisplay-frame-panes frame)
          ;; Auto-probe durations for tracks with unknown runtime
          (let ((updated 0))
            (dolist (track (playlist-elements playlist))
              (when (or (null (runtime track)) (< (runtime track) 0))
                (let ((duration (get-audio-duration (track-path track))))
                  (when duration
                    (setf (runtime track) duration)
                    (incf updated)))))
            (compute-playlist-duration playlist)
            (setf (frame-message frame)
                  (format nil "Loaded ~A (~D durations probed)" (file-namestring path) updated))))
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
        (setf (add-playlist-element playlist (1+ (frame-selected-index frame))) track)
        (incf (frame-selected-index frame))
        (setf (frame-message frame) 
              (format nil "Added: ~A" (file-namestring path)))
        (redisplay-frame-panes frame)))))

;; Browse: enter interactive browser mode
(define-playlisp-editor-command (com-browse :name "Browse")
    ()
  (%log "COM-BROWSE called")
  (let* ((frame *application-frame*)
         (stream (or (get-frame-pane frame 'interactor)
                     (frame-standard-input frame)))
         (port (port (frame-manager frame))))
    (%log (format nil "COM-BROWSE stream=~S port=~S" stream port))
    ;; Use charmed-read-line which bypasses DREI and echoes directly
    (let ((dir-str (clim-charmed:charmed-read-line
                    port stream :prompt "Directory: ")))
      (%log (format nil "COM-BROWSE dir-str=~S" dir-str))
      (when (and dir-str (> (length dir-str) 0))
        (enter-browse-mode frame (pathname dir-str))))))

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
          ;; Navigate into directory (keep path as string to avoid bracket issues)
          (let ((new-dir (browser-entry-path entry)))
            (setf (frame-browser-dir frame) new-dir
                  (frame-browser-entries frame) (browser-refresh-entries new-dir)
                  (frame-browser-cursor frame) 0
                  (frame-browser-scroll frame) 0))
          ;; Add audio file to playlist
          (let* ((path (browser-entry-path entry))
                 (name (browser-entry-name entry))
                 (root (frame-browser-root frame))
                 (rel-title (relative-path-title path root))
                 (playlist (frame-playlist frame)))
            (when playlist
              (let ((track (make-track-from-file path :title rel-title)))
                (setf (add-playlist-element playlist (1+ (frame-selected-index frame))) track)
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
      (let ((root (frame-browser-root frame)))
        (dolist (entry selected)
          (let* ((path (browser-entry-path entry))
                 (rel-title (relative-path-title path root))
                 (track (make-track-from-file path :title rel-title)))
            (setf (add-playlist-element playlist (+ (frame-selected-index frame) count 1)) track)
            (incf count))))
      (incf (frame-selected-index frame) count)
      ;; Clear selections
      (dolist (e entries)
        (setf (browser-entry-selected-p e) nil))
      (setf (frame-message frame)
            (format nil "Added ~D tracks" count))
      (redisplay-frame-panes frame)
      (render-browser-to-interactor frame))))

(defun relative-path-title (path root)
  "Compute PATH relative to ROOT using string operations (avoids pathname wildcard issues)."
  (let ((root-str (if (stringp root) root (namestring root)))
        (path-str (if (stringp path) path (namestring path))))
    (if (and (>= (length path-str) (length root-str))
             (string= path-str root-str :end1 (length root-str)))
        (subseq path-str (length root-str))
        path-str)))

(defun parent-directory-string (dir-str)
  "Compute parent directory from a directory path string."
  (let* ((trimmed (string-right-trim "/" dir-str))
         (slash-pos (position #\/ trimmed :from-end t)))
    (when slash-pos
      (subseq dir-str 0 (1+ slash-pos)))))

(define-playlisp-editor-command (com-browser-back :name "Nav Back")
    ()
  (let* ((frame *application-frame*)
         (dir (if (stringp (frame-browser-dir frame))
                  (frame-browser-dir frame)
                  (namestring (frame-browser-dir frame)))))
    (when (and (frame-browse-mode-p frame) dir)
      (let ((parent (parent-directory-string dir)))
        (when (and parent (not (string= parent dir)))
          (setf (frame-browser-dir frame) parent
                (frame-browser-entries frame) (browser-refresh-entries parent)
                (frame-browser-cursor frame) 0
                (frame-browser-scroll frame) 0)
          (render-browser-to-interactor frame))))))

(define-playlisp-editor-command (com-browser-close :name "Nav Close")
    ()
  (when (frame-browse-mode-p *application-frame*)
    (exit-browse-mode *application-frame*)))

;; Refresh duration for all tracks with missing/unknown durations using ffprobe
(define-playlisp-editor-command (com-refresh-duration :name "Refresh Duration" :keystroke (#\r :control))
    ()
  (let* ((frame *application-frame*)
         (playlist (frame-playlist frame))
         (tracks (when playlist (playlist-elements playlist)))
         (updated 0))
    (dolist (track tracks)
      (when (or (null (runtime track)) (< (runtime track) 0))
        (let ((duration (get-audio-duration (track-path track))))
          (when duration
            (setf (runtime track) duration)
            (incf updated)))))
    (when playlist
      (compute-playlist-duration playlist))
    (setf (frame-message frame)
          (format nil "Refreshed ~D track~:P" updated))
    (redisplay-frame-panes frame)))

;; Help - clear interactor then print help so prompt stays accessible
(define-playlisp-editor-command (com-help :name "Help")
    ()
  (let* ((frame *application-frame*)
         (stream (get-frame-pane frame 'interactor))
         (fm (frame-manager frame))
         (port (when fm (port fm))))
    ;; Clear the interactor screen area
    (when (and stream port (typep port 'clim-charmed::charmed-port))
      (let ((screen (clim-charmed::charmed-port-screen port))
            (vp (gethash stream (clim-charmed::charmed-port-viewport-sizes port))))
        (when (and screen vp)
          (charmed:screen-fill-rect screen
                                    (round (first vp))
                                    (round (second vp))
                                    (round (third vp))
                                    (round (fourth vp)))))
      (setf (clim-charmed::pane-scroll-offset port stream) 0))
    (when stream (window-clear stream))
    ;; Print compact help
    (let ((s (frame-standard-output frame)))
      (with-drawing-options (s :ink +cyan+)
        (format s "Ctrl-N/P next/prev  Ctrl-A first  Ctrl-U/D move  Ctrl-X del  Ctrl-R refresh~%"))
      (with-drawing-options (s :ink +cyan+)
        (format s "Ctrl-E edit mode  Ctrl-S save  Ctrl-Q quit~%"))
      (with-drawing-options (s :ink +yellow+)
        (format s "Edit: ↑↓ nav  u/d move  x del  ESC exit~%"))
      (with-drawing-options (s :ink +yellow+)
        (format s "Browser: ↑↓ nav  Enter open  Space star  a add  Bksp back  ESC close~%"))
      (with-drawing-options (s :ink +gray50+)
        (format s "Commands: Open Add Browse Save New Edit Metadata Help Last Quit~%")))))


;; Quit
(define-playlisp-editor-command (com-quit :name "Quit" :keystroke (:q :control))
    ()
  (frame-exit *application-frame*))

;; Exit SBCL completely
(define-playlisp-editor-command (com-exit :name "Exit")
    ()
  (frame-exit *application-frame*))

;;; ── Raw Key Dispatch (Browse & Edit Modes) ──────────────────────────

;;; When charmed-frame-wants-raw-keys-p returns T (browse or edit mode),
;;; the charmed port routes keys to the frame's event queue via queue-append.
;;; queue-read drives process-next-event to pump terminal input.
(defun dispatch-common-keys (key-name char mods)
  "Handle keys common to both browse and edit modes. Returns a command list or NIL."
  (cond
    ;; Ctrl-Q quits from any raw-key mode
    ((and (eql key-name :|Q|)
          (not (zerop (logand mods +control-key+))))
     '(com-quit))
    ;; Ctrl-S saves from any raw-key mode
    ((and (eql key-name :|S|)
          (not (zerop (logand mods +control-key+))))
     '(com-save))
    (t nil)))

(defun dispatch-browse-key (key-name char mods)
  "Dispatch a key event in browse mode. Returns a command list or NIL."
  (or (dispatch-common-keys key-name char mods)
      (cond
        ((eql key-name :down)      '(com-browser-down))
        ((eql key-name :up)        '(com-browser-up))
        ;; Ctrl-N / Ctrl-P: down/up (consistent with normal mode)
        ((and (eql key-name :|N|)
              (not (zerop (logand mods +control-key+))))
         '(com-browser-down))
        ((and (eql key-name :|P|)
              (not (zerop (logand mods +control-key+))))
         '(com-browser-up))
        ((eql key-name :newline)   '(com-browser-enter))
        ((eql key-name :return)    '(com-browser-enter))
        ((eql key-name :backspace) '(com-browser-back))
        ((eql key-name :escape)    '(com-browser-close))
        ((eql char #\Space)        '(com-browser-space))
        ((eql char #\a)            '(com-browser-add-selected))
        (t nil))))

(defun dispatch-edit-key (key-name char mods)
  "Dispatch a key event in edit mode. Returns a command list or NIL.
   ↑/↓ navigate, u/d move track up/down, x/Delete delete, ESC exit."
  (or (dispatch-common-keys key-name char mods)
      (cond
        ;; Plain Up/Down: navigate
        ((eql key-name :up)                '(com-edit-up))
        ((eql key-name :down)              '(com-edit-down))
        ;; Ctrl-N / Ctrl-P: down/up (consistent with normal mode)
        ((and (eql key-name :|N|)
              (not (zerop (logand mods +control-key+))))
         '(com-edit-down))
        ((and (eql key-name :|P|)
              (not (zerop (logand mods +control-key+))))
         '(com-edit-up))
        ;; u/d: move track up/down
        ((eql char #\u)                    '(com-edit-move-up))
        ((eql char #\d)                    '(com-edit-move-down))
        ;; Delete track
        ((eql char #\x)                    '(com-edit-delete))
        ((eql key-name :delete)            '(com-edit-delete))
        ;; ESC: exit edit mode
        ((eql key-name :escape)            '(com-edit-close))
        (t nil))))

(defmethod read-frame-command ((frame playlisp-editor) &key stream)
  (declare (ignore stream))
  (cond
    ((or (frame-browse-mode-p frame) (frame-edit-mode-p frame))
     (let* ((port (port (frame-manager frame)))
            (queue (climi::frame-event-queue frame))
            (browse-p (frame-browse-mode-p frame)))
       ;; The frame's event queue is a concurrent-queue (on SBCL).
       ;; We must call process-next-event to pump terminal input, then
       ;; check if raw-key events arrived in the frame queue.
       (loop
         (process-next-event port :timeout nil)
         (let ((event (climi::queue-read-no-hang queue)))
           (when (and event (typep event 'key-press-event))
             (let* ((key-name (keyboard-event-key-name event))
                    (char (keyboard-event-character event))
                    (mods (event-modifier-state event))
                    (cmd (if browse-p
                             (dispatch-browse-key key-name char mods)
                             (dispatch-edit-key key-name char mods))))
               (when cmd (return cmd))))))))
    (t (call-next-method))))

;;; ── Standard Output ──────────────────────────────────────────────────

(defmethod frame-standard-output ((frame playlisp-editor))
  (get-frame-pane frame 'interactor))

;;; ── Diagnostics (TEMPORARY) ──────────────────────────────────────────

(defun %log (msg)
  (with-open-file (s "/tmp/charmed-diag.log"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format s "~A ~A~%" (get-internal-real-time) msg)
    (finish-output s)))

(defmethod clim:enable-frame :around ((frame playlisp-editor))
  (%log "ENABLE-FRAME :around enter")
  (call-next-method)
  (%log "ENABLE-FRAME :around exit"))

(defmethod clim:frame-query-io :around ((frame playlisp-editor))
  (%log "FRAME-QUERY-IO called")
  (let ((input (slot-value frame 'climi::input-pane)))
    (%log (format nil "FRAME-QUERY-IO slot input-pane=~S" input))
    (if input
        (progn (%log "FRAME-QUERY-IO returning input-pane") input)
        (let ((output (slot-value frame 'climi::output-pane)))
          (%log (format nil "FRAME-QUERY-IO slot output-pane=~S" output))
          (if output
              (progn (%log "FRAME-QUERY-IO returning output-pane") output)
              (progn (%log "FRAME-QUERY-IO both nil, calling next method")
                     (call-next-method)))))))

(defmethod run-frame-top-level :around ((frame playlisp-editor) &key &allow-other-keys)
  (%log "RUN-FRAME-TOP-LEVEL :around (playlisp) ENTER")
  (unwind-protect
       (call-next-method)
    (%log "RUN-FRAME-TOP-LEVEL :around (playlisp) EXIT")))

;;; ── Entry Point ──────────────────────────────────────────────────────

(defun run (&optional filepath)
  "Launch the playlisp McCLIM TUI editor.
   If FILEPATH is given, load the M3U playlist from that path."
  (let* ((port (clim:find-port :server-path '(:charmed)))
         (fm (find-frame-manager :port port)))
    (unwind-protect
         (let ((frame (make-application-frame 'playlisp-editor
                                              :frame-manager fm)))
           ;; Load playlist if given
           (when filepath
             (handler-case
                 (let ((playlist (parse-m3u-file filepath)))
                   (setf (frame-playlist frame) playlist
                         (frame-filepath frame) (namestring (truename filepath)))
                   ;; Auto-probe durations for tracks with unknown runtime
                   (dolist (track (playlist-elements playlist))
                     (when (or (null (runtime track)) (< (runtime track) 0))
                       (let ((duration (get-audio-duration (track-path track))))
                         (when duration
                           (setf (runtime track) duration)))))
                   (compute-playlist-duration playlist))
               (error (e)
                 (setf (frame-message frame)
                       (format nil "Load error: ~A" e)))))
           ;; Default to empty playlist
           (unless (frame-playlist frame)
             (setf (frame-playlist frame) (make-playlist "Untitled")))
           ;; Run (with diagnostics)
           (with-open-file (s "/tmp/charmed-diag.log"
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
             (format s "PRE-RUN state=~S~%" (clim:frame-state frame))
             (finish-output s))
           (handler-case
               (progn
                 (run-frame-top-level frame)
                 (with-open-file (s "/tmp/charmed-diag.log"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
                   (format s "POST-RUN returned normally~%")
                   (finish-output s)))
             (serious-condition (e)
               (with-open-file (s "/tmp/charmed-diag.log"
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
                 (format s "RUN-CONDITION ~S: ~A~%" (type-of e) e)
                 (finish-output s)))))
      (destroy-port port)))
  (uiop:quit 0))
