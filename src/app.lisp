;;; src/app.lisp - Main TUI application for playlisp
;;; Event loop, keybindings, playlist editing via m3u-operations

(uiop:define-package #:playlisp/src/app
  (:use #:cl
        #:playlisp/src/ansi
        #:playlisp/src/terminal
        #:playlisp/src/widgets
        #:playlisp/src/layout
        #:playlisp/parser
        #:playlisp/m3u-operations)
  (:export #:run-tui
           #:*app*))

(in-package #:playlisp/src/app)

;;; ── Application state ─────────────────────────────────────────────

(defclass app-state ()
  ((layout    :accessor app-layout    :initform nil)
   (playlist  :accessor app-playlist  :initform nil)
   (filepath  :accessor app-filepath  :initform nil
              :documentation "Path to the M3U file being edited")
   (mode      :accessor app-mode      :initform :normal
              :documentation ":normal, :command, :help, :browse")
   (running-p :accessor app-running-p :initform t)
   (message   :accessor app-message   :initform nil
              :documentation "Transient status message")
   (active-pane :accessor app-active-pane :initform :tracklist
                :documentation ":tracklist or :details")
   (browser   :accessor app-browser   :initform nil
              :documentation "File browser panel instance (created on demand)"))
  (:documentation "Global application state for the TUI"))

(defvar *app* nil "Current application state")

;;; ── Helpers ───────────────────────────────────────────────────────

(defun current-tracklist ()
  "Return the tracklist panel from the current layout."
  (when (app-layout *app*)
    (layout-tracklist (app-layout *app*))))

(defun current-details ()
  "Return the details panel from the current layout."
  (when (app-layout *app*)
    (layout-details (app-layout *app*))))

(defun current-tracks ()
  "Return the list of tracks from the current playlist."
  (when (app-playlist *app*)
    (playlist-elements (app-playlist *app*))))

(defun track-count ()
  (length (current-tracks)))

(defun selected-track ()
  "Return the currently selected track, or NIL."
  (let ((tl (current-tracklist)))
    (when (and tl (app-playlist *app*))
      (let ((tracks (current-tracks))
            (idx (tracklist-panel-cursor tl)))
        (when (and tracks (< idx (length tracks)))
          (nth idx tracks))))))

(defun sync-details ()
  "Update the details panel to show the currently selected track."
  (let ((dp (current-details))
        (track (selected-track)))
    (when dp
      (setf (details-panel-track dp) track
            (panel-dirty-p dp) t))))

(defun sync-pane-focus ()
  "Update active-p on panels to match app-active-pane."
  (let ((tl (current-tracklist))
        (dp (current-details)))
    (when tl (setf (panel-active-p tl)
                   (eq (app-active-pane *app*) :tracklist)))
    (when dp (setf (panel-active-p dp)
                   (eq (app-active-pane *app*) :details)))))

;;; ── Cursor movement ───────────────────────────────────────────────

(defun move-cursor-up ()
  (let ((tl (current-tracklist)))
    (when (and tl (> (tracklist-panel-cursor tl) 0))
      (decf (tracklist-panel-cursor tl))
      ;; Scroll if cursor went above viewport
      (when (< (tracklist-panel-cursor tl)
               (tracklist-panel-scroll-offset tl))
        (setf (tracklist-panel-scroll-offset tl)
              (tracklist-panel-cursor tl)))
      (sync-details))))

(defun move-cursor-down ()
  (let ((tl (current-tracklist)))
    (when (and tl (< (tracklist-panel-cursor tl)
                     (1- (track-count))))
      (incf (tracklist-panel-cursor tl))
      ;; Scroll if cursor went below viewport
      (let ((vis (tracklist-visible-height tl)))
        (when (>= (tracklist-panel-cursor tl)
                  (+ (tracklist-panel-scroll-offset tl) vis))
          (setf (tracklist-panel-scroll-offset tl)
                (1+ (- (tracklist-panel-cursor tl) vis)))))
      (sync-details))))

(defun move-cursor-top ()
  (let ((tl (current-tracklist)))
    (when tl
      (setf (tracklist-panel-cursor tl) 0
            (tracklist-panel-scroll-offset tl) 0)
      (sync-details))))

(defun move-cursor-bottom ()
  (let ((tl (current-tracklist)))
    (when (and tl (current-tracks))
      (let* ((last-idx (1- (track-count)))
             (vis (tracklist-visible-height tl)))
        (setf (tracklist-panel-cursor tl) last-idx
              (tracklist-panel-scroll-offset tl)
              (max 0 (1+ (- last-idx vis))))
        (sync-details)))))

(defun page-up ()
  (let ((tl (current-tracklist)))
    (when tl
      (let ((vis (tracklist-visible-height tl)))
        (setf (tracklist-panel-cursor tl)
              (max 0 (- (tracklist-panel-cursor tl) vis)))
        (setf (tracklist-panel-scroll-offset tl)
              (max 0 (- (tracklist-panel-scroll-offset tl) vis)))
        (sync-details)))))

(defun page-down ()
  (let ((tl (current-tracklist)))
    (when (and tl (current-tracks))
      (let* ((vis (tracklist-visible-height tl))
             (max-idx (1- (track-count))))
        (setf (tracklist-panel-cursor tl)
              (min max-idx (+ (tracklist-panel-cursor tl) vis)))
        (setf (tracklist-panel-scroll-offset tl)
              (min (max 0 (1+ (- max-idx vis)))
                   (+ (tracklist-panel-scroll-offset tl) vis)))
        (sync-details)))))

;;; ── Playlist operations ───────────────────────────────────────────

(defun delete-selected-track ()
  "Remove the selected track from the playlist."
  (let* ((tl (current-tracklist))
         (pl (app-playlist *app*))
         (tracks (current-tracks))
         (idx (when tl (tracklist-panel-cursor tl))))
    (when (and pl tracks idx (< idx (length tracks)))
      (let ((removed (nth idx tracks)))
        ;; Remove from list
        (setf (playlist-elements pl)
              (append (subseq tracks 0 idx)
                      (nthcdr (1+ idx) tracks)))
        ;; Renumber remaining tracks
        (loop for track in (playlist-elements pl)
              for i from 1
              do (setf (qnumber track) i))
        ;; Adjust cursor if at end
        (when (>= idx (length (playlist-elements pl)))
          (setf (tracklist-panel-cursor tl)
                (max 0 (1- (length (playlist-elements pl))))))
        ;; Update panels
        (setf (tracklist-panel-playlist tl) pl)
        (sync-details)
        (setf (app-message *app*)
              (format nil "Removed: ~A" (or (title removed) "track")))))))

(defun move-track-up ()
  "Swap the selected track with the one above it."
  (let* ((tl (current-tracklist))
         (pl (app-playlist *app*))
         (tracks (current-tracks))
         (idx (when tl (tracklist-panel-cursor tl))))
    (when (and pl tracks idx (> idx 0))
      (rotatef (nth idx tracks) (nth (1- idx) tracks))
      ;; Renumber
      (loop for track in tracks for i from 1 do (setf (qnumber track) i))
      (decf (tracklist-panel-cursor tl))
      (when (< (tracklist-panel-cursor tl)
               (tracklist-panel-scroll-offset tl))
        (setf (tracklist-panel-scroll-offset tl)
              (tracklist-panel-cursor tl)))
      (sync-details))))

(defun move-track-down ()
  "Swap the selected track with the one below it."
  (let* ((tl (current-tracklist))
         (pl (app-playlist *app*))
         (tracks (current-tracks))
         (idx (when tl (tracklist-panel-cursor tl))))
    (when (and pl tracks idx (< idx (1- (length tracks))))
      (rotatef (nth idx tracks) (nth (1+ idx) tracks))
      ;; Renumber
      (loop for track in tracks for i from 1 do (setf (qnumber track) i))
      (incf (tracklist-panel-cursor tl))
      (let ((vis (tracklist-visible-height tl)))
        (when (>= (tracklist-panel-cursor tl)
                  (+ (tracklist-panel-scroll-offset tl) vis))
          (setf (tracklist-panel-scroll-offset tl)
                (1+ (- (tracklist-panel-cursor tl) vis)))))
      (sync-details))))

;;; ── File browser ─────────────────────────────────────────────────

(defun open-browser (&optional start-dir &key recursive)
  "Open the file browser overlay. Starts at START-DIR, or *music-library-path*,
   or current directory if neither is set. If RECURSIVE is T, show all audio files recursively."
  (let* ((dir (or start-dir
                  *music-library-path*
                  (uiop:getcwd)))
         (lay (app-layout *app*))
         (w (layout-width lay))
         (h (layout-height lay))
         ;; Browser overlays the right pane area, or center if narrow
         (bw (max 40 (floor (* w 0.55))))
         (bh (- h 2))
         (bx (max 1 (floor (- w bw) 2)))
         (by 1)
         (browser (make-instance 'file-browser-panel
                                 :x bx :y by
                                 :width bw :height bh
                                 :current-dir dir
                                 :recursive-p recursive)))
    (browser-refresh browser)
    (setf (app-browser *app*) browser
          (app-mode *app*) :browse)))

(defun browser-move-up ()
  (let ((br (app-browser *app*)))
    (when (and br (> (browser-cursor br) 0))
      (decf (browser-cursor br))
      (when (< (browser-cursor br) (browser-scroll-offset br))
        (setf (browser-scroll-offset br) (browser-cursor br))))))

(defun browser-move-down ()
  (let ((br (app-browser *app*)))
    (when (and br (< (browser-cursor br)
                     (1- (length (browser-entries br)))))
      (incf (browser-cursor br))
      (let ((vis (browser-visible-height br)))
        (when (>= (browser-cursor br)
                  (+ (browser-scroll-offset br) vis))
          (setf (browser-scroll-offset br)
                (1+ (- (browser-cursor br) vis))))))))

(defun browser-enter ()
  "Enter a directory or add the selected audio file to the playlist."
  (let* ((br (app-browser *app*))
         (entry (when br (browser-selected-entry br))))
    (when entry
      (if (browser-entry-dir-p entry)
          ;; Navigate into directory
          (progn
            (setf (browser-current-dir br)
                  (pathname (browser-entry-path entry)))
            (browser-refresh br))
          ;; Add audio file to playlist
          (browser-add-selected entry)))))

(defun browser-add-selected (entry)
  "Add the browser ENTRY as a track after the current cursor position."
  (let* ((path (browser-entry-path entry))
         (name (browser-entry-name entry))
         (duration (get-audio-duration path))
         (pl (app-playlist *app*))
         (tl (current-tracklist))
         (tracks (playlist-elements pl))
         (cursor-idx (if tl (tracklist-panel-cursor tl) 0))
         (insert-idx (min (length tracks) (1+ cursor-idx)))
         (new-track (make-track (if (string= name "") path name) path
                                :runtime (or duration -1))))
    ;; Insert
    (setf (playlist-elements pl)
          (append (subseq tracks 0 insert-idx)
                  (list new-track)
                  (nthcdr insert-idx tracks)))
    ;; Renumber
    (loop for track in (playlist-elements pl)
          for i from 1
          do (setf (qnumber track) i))
    ;; Move tracklist cursor to new track
    (when tl
      (setf (tracklist-panel-cursor tl) insert-idx))
    (sync-details)
    (setf (app-message *app*)
          (format nil "Added: ~A" name))))

(defun browser-add-all-selected ()
  "Add all selected files to the playlist. If none selected, add current entry."
  (let* ((br (app-browser *app*))
         (selected (browser-selected-files br)))
    (if (null selected)
        ;; No selection - add current entry if it's a file
        (let ((entry (browser-selected-entry br)))
          (when (and entry (not (browser-entry-dir-p entry)))
            (browser-add-selected entry)))
        ;; Add all selected files
        (let ((count 0))
          (dolist (entry selected)
            (let* ((path (browser-entry-path entry))
                   (name (browser-entry-name entry))
                   (duration (get-audio-duration path))
                   (pl (app-playlist *app*))
                   (tracks (playlist-elements pl))
                   (new-track (make-track (if (string= name "") path name) path
                                          :runtime (or duration -1))))
              ;; Append to end
              (setf (playlist-elements pl)
                    (append tracks (list new-track)))
              (incf count)))
          ;; Renumber all tracks
          (loop for track in (playlist-elements (app-playlist *app*))
                for i from 1
                do (setf (qnumber track) i))
          ;; Clear selections
          (dolist (entry (browser-entries br))
            (setf (browser-entry-selected-p entry) nil))
          (sync-details)
          (setf (app-message *app*)
                (format nil "Added ~D tracks" count))))))

(defun browser-page-up ()
  (let ((br (app-browser *app*)))
    (when br
      (let ((vis (browser-visible-height br)))
        (setf (browser-cursor br)
              (max 0 (- (browser-cursor br) vis)))
        (setf (browser-scroll-offset br)
              (max 0 (- (browser-scroll-offset br) vis)))))))

(defun browser-page-down ()
  (let ((br (app-browser *app*)))
    (when br
      (let ((vis (browser-visible-height br))
            (max-idx (1- (length (browser-entries br)))))
        (setf (browser-cursor br)
              (min max-idx (+ (browser-cursor br) vis)))
        (setf (browser-scroll-offset br)
              (min (max 0 (1+ (- max-idx vis)))
                   (+ (browser-scroll-offset br) vis)))))))

(defun render-browser ()
  "Render the file browser overlay on top of the normal UI."
  (let ((br (app-browser *app*)))
    (when br
      (begin-sync-update)
      (panel-render br)
      ;; Mini status hint at bottom of browser
      (let ((bx (panel-x br))
            (by (+ (panel-y br) (panel-height br) -1))
            (bw (panel-width br)))
        (cursor-to (1+ by) bx)
        (bg :status)
        (fg :bright-black)
        (let ((hint "Enter:add/open  Esc:close  j/k:nav"))
          (princ hint *terminal-io*)
          (let ((pad (- bw (length hint))))
            (when (> pad 0)
              (princ (make-string pad :initial-element #\Space) *terminal-io*)))))
      (reset)
      (end-sync-update)
      (force-output *terminal-io*))))

;;; ── Text input prompt ─────────────────────────────────────────────

(defun read-input-line (prompt)
  "Show PROMPT on the status bar and read a line of text character by character.
   Returns the string, or NIL if the user pressed Escape to cancel."
  (let ((buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (y (layout-status-y (app-layout *app*)))
        (w (layout-width (app-layout *app*))))
    (labels ((redraw-prompt ()
               (cursor-to y 1)
               (bg :status)
               (fg :yellow)
               (bold)
               (format *terminal-io* " ~A" prompt)
               (reset)
               (bg :status)
               (fg :white)
               (princ buf *terminal-io*)
               ;; Clear rest of line
               (let ((used (+ 1 (length prompt) (length buf))))
                 (when (< used w)
                   (princ (make-string (- w used) :initial-element #\Space)
                          *terminal-io*)))
               (reset)
               (cursor-to y (+ 2 (length prompt) (length buf)))
               (cursor-show)
               (force-output *terminal-io*)))
      (redraw-prompt)
      (loop
        (let ((key (read-key)))
          (when key
            (let ((code (key-event-code key))
                  (ch (key-event-char key)))
              (cond
                ;; Cancel
                ((eq code +key-escape+)
                 (cursor-hide)
                 (return nil))
                ;; Submit
                ((eq code +key-enter+)
                 (cursor-hide)
                 (return (coerce buf 'string)))
                ;; Backspace
                ((eq code +key-backspace+)
                 (when (> (fill-pointer buf) 0)
                   (vector-pop buf)
                   (redraw-prompt)))
                ;; Printable character
                ((and ch (graphic-char-p ch))
                 (vector-push-extend ch buf)
                 (redraw-prompt))))))))))

;;; ── Add track ─────────────────────────────────────────────────────

(defun add-track-at (position)
  "Prompt for a file path and insert a new track at POSITION.
   POSITION is :after (after cursor) or :before (before cursor)."
  (let ((path (read-input-line "Add track path: ")))
    (when (and path (> (length path) 0))
      (let* ((pl (app-playlist *app*))
             (tl (current-tracklist))
             (tracks (playlist-elements pl))
             (cursor-idx (if tl (tracklist-panel-cursor tl) 0))
             (insert-idx (ecase position
                           (:after  (min (length tracks) (1+ cursor-idx)))
                           (:before cursor-idx)))
             (name (file-namestring path))
             (new-track (make-track (if (string= name "") path name)
                                    path)))
        ;; Insert into list
        (setf (playlist-elements pl)
              (append (subseq tracks 0 insert-idx)
                      (list new-track)
                      (nthcdr insert-idx tracks)))
        ;; Renumber
        (loop for track in (playlist-elements pl)
              for i from 1
              do (setf (qnumber track) i))
        ;; Move cursor to new track
        (when tl
          (setf (tracklist-panel-cursor tl) insert-idx))
        (sync-details)
        (setf (app-message *app*)
              (format nil "Added: ~A" (or name path)))))))

;;; ── Save playlist ─────────────────────────────────────────────────

(defun write-m3u-file (playlist filepath)
  "Write PLAYLIST to FILEPATH in extended M3U format."
  (with-open-file (out filepath :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
    (format out "#EXTM3U~%")
    ;; Metadata headers
    (when (playlist-name playlist)
      (format out "#PLAYLIST:~A~%" (playlist-name playlist)))
    (when (playlist-phase playlist)
      (format out "#PHASE:~A~%" (playlist-phase playlist)))
    (when (playlist-duration playlist)
      (format out "#DURATION:~A~%" (playlist-duration playlist)))
    (when (playlist-curator playlist)
      (format out "#CURATOR:~A~%" (playlist-curator playlist)))
    (when (playlist-description playlist)
      (format out "#DESCRIPTION:~A~%" (playlist-description playlist)))
    ;; Tracks
    (dolist (track (playlist-elements playlist))
      (format out "#EXTINF:~A,~A~%"
              (or (runtime track) -1)
              (or (title track) ""))
      (format out "~A~%" (or (track-path track) "")))))

(defun save-playlist ()
  "Save the current playlist. Prompts for path if none set."
  (let ((pl (app-playlist *app*))
        (path (app-filepath *app*)))
    (unless path
      (setf path (read-input-line "Save as: ")))
    (when (and path (> (length path) 0))
      (handler-case
          (progn
            ;; Auto-calculate total duration before saving
            (let ((total-secs (calculate-total-duration pl)))
              (when total-secs
                (setf (playlist-duration pl) (format-duration-human total-secs))))
            (write-m3u-file pl path)
            (setf (app-filepath *app*) path
                  (app-message *app*)
                  (format nil "Saved: ~A (~D tracks)" path (track-count))))
        (error (e)
          (setf (app-message *app*)
                (format nil "Save error: ~A" e)))))))

(defun calculate-total-duration (playlist)
  "Calculate total duration of all tracks in PLAYLIST. Returns seconds or NIL."
  (let ((total 0)
        (has-duration nil))
    (dolist (track (playlist-elements playlist))
      (let ((rt (runtime track)))
        (when (and rt (numberp rt) (> rt 0))
          (incf total rt)
          (setf has-duration t))))
    (when has-duration total)))

(defun format-duration-human (seconds)
  "Format SECONDS as human-readable duration (e.g., '1 hour 23 minutes')."
  (let* ((hours (floor seconds 3600))
         (mins (floor (mod seconds 3600) 60)))
    (cond
      ((>= hours 1)
       (format nil "~D hour~:P ~D minute~:P" hours mins))
      (t
       (format nil "~D minute~:P" mins)))))

;;; ── Edit playlist metadata ────────────────────────────────────────

(defun edit-playlist-metadata ()
  "Prompt to edit playlist metadata fields."
  (let ((pl (app-playlist *app*)))
    (when pl
      ;; Edit playlist name
      (let ((new-name (read-input-line
                       (format nil "Playlist name [~A]: "
                               (or (playlist-name pl) "")))))
        (when (and new-name (> (length new-name) 0))
          (setf (playlist-name pl) new-name)))
      (full-render)
      ;; Edit phase
      (let ((new-phase (read-input-line
                        (format nil "Phase [~A]: "
                                (or (playlist-phase pl) "")))))
        (when (and new-phase (> (length new-phase) 0))
          (setf (playlist-phase pl) new-phase)))
      (full-render)
      ;; Edit duration
      (let ((new-duration (read-input-line
                           (format nil "Duration [~A]: "
                                   (or (playlist-duration pl) "")))))
        (when (and new-duration (> (length new-duration) 0))
          (setf (playlist-duration pl) new-duration)))
      (full-render)
      ;; Edit curator
      (let ((new-curator (read-input-line
                          (format nil "Curator [~A]: "
                                  (or (playlist-curator pl) "")))))
        (when (and new-curator (> (length new-curator) 0))
          (setf (playlist-curator pl) new-curator)))
      (full-render)
      ;; Edit description
      (let ((new-desc (read-input-line
                       (format nil "Description [~A]: "
                               (or (playlist-description pl) "")))))
        (when (and new-desc (> (length new-desc) 0))
          (setf (playlist-description pl) new-desc)))
      (setf (app-message *app*) "Metadata updated")
      (full-render))))

;;; ── Open playlist ─────────────────────────────────────────────────

(defun open-playlist ()
  "Prompt for a file path and load an M3U playlist."
  (let ((path (read-input-line "Open file: ")))
    (when (and path (> (length path) 0))
      (handler-case
          (let ((pl (parse-m3u-file path)))
            (setf (app-playlist *app*) pl
                  (app-filepath *app*) path)
            (let ((tl (current-tracklist)))
              (when tl
                (setf (tracklist-panel-cursor tl) 0
                      (tracklist-panel-scroll-offset tl) 0)))
            (sync-details)
            (setf (app-message *app*)
                  (format nil "Loaded: ~A (~D tracks)"
                          path (track-count))))
        (error (e)
          (setf (app-message *app*)
                (format nil "Open error: ~A" e)))))))

;;; ── Refresh durations ────────────────────────────────────────────

(defun unescape-path (path)
  "Remove backslash escapes from PATH (e.g., \\[ becomes [)."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length path))
          do (let ((c (char path i)))
               (if (and (char= c #\\)
                        (< (1+ i) (length path)))
                   (progn
                     (write-char (char path (1+ i)) out)
                     (incf i 2))
                   (progn
                     (write-char c out)
                     (incf i)))))))

(defun file-exists-p (path)
  "Check if PATH exists without triggering wildcard interpretation.
   Uses shell test -f to avoid SBCL treating brackets as wildcards."
  (handler-case
      (let ((output (uiop:run-program (list "test" "-f" path)
                                      :ignore-error-status t)))
        (declare (ignore output))
        (zerop (nth-value 2 (uiop:run-program (list "test" "-f" path)
                                              :ignore-error-status t))))
    (error () nil)))

(defun refresh-all-durations ()
  "Re-scan all tracks and update their durations using ffprobe.
   Handles backslash-escaped paths (e.g., \\[FLAC]) in M3U files."
  (let ((pl (app-playlist *app*)))
    (when pl
      (let ((count 0)
            (total (length (playlist-elements pl))))
        (dolist (track (playlist-elements pl))
          (let* ((raw-path (track-path track))
                 (path (when raw-path (unescape-path raw-path))))
            (when (and path (file-exists-p path))
              (let ((duration (get-audio-duration path)))
                (when duration
                  (setf (runtime track) duration)
                  (incf count))))))
        (setf (app-message *app*)
              (format nil "Updated ~D/~D track durations" count total))))))

;;; ── Render ────────────────────────────────────────────────────────

(defun total-playlist-duration ()
  "Calculate total duration of all tracks in seconds. Returns NIL if no valid durations."
  (let ((pl (app-playlist *app*)))
    (when pl
      (let ((total 0)
            (has-duration nil))
        (dolist (track (playlist-elements pl))
          (let ((rt (runtime track)))
            (when (and rt (numberp rt) (> rt 0))
              (incf total rt)
              (setf has-duration t))))
        (when has-duration total)))))

(defun full-render ()
  "Clear and render the full UI."
  (clear-screen)
  (sync-pane-focus)
  (let* ((tl (current-tracklist))
         (pl (app-playlist *app*)))
    (when (and tl pl)
      (setf (tracklist-panel-playlist tl) pl))
    (layout-render-all (app-layout *app*)
                       :mode (string-upcase (symbol-name (app-mode *app*)))
                       :filename (app-filepath *app*)
                       :track-count (track-count)
                       :total-duration (total-playlist-duration))
    ;; Show transient message if any
    (when (app-message *app*)
      (let ((y (layout-status-y (app-layout *app*))))
        (cursor-to y 1)
        (bg :status)
        (fg :yellow)
        (bold)
        (format *terminal-io* " ~A" (app-message *app*))
        (reset)
        (force-output *terminal-io*)))))

;;; ── Help overlay ──────────────────────────────────────────────────

(defun show-help ()
  "Display a help overlay in the center of the screen."
  (let* ((w (layout-width (app-layout *app*)))
         (h (layout-height (app-layout *app*)))
         (box-w (min 50 (- w 4)))
         (box-h 20)
         (bx (max 1 (floor (- w box-w) 2)))
         (by (max 1 (floor (- h box-h) 2)))
         (help-lines '("  j/↓     Move down"
                       "  k/↑     Move up"
                       "  g       Go to top"
                       "  G       Go to bottom"
                       "  PgUp    Page up"
                       "  PgDn    Page down"
                       "  Tab     Switch pane"
                       "  a       Add track after cursor"
                       "  A       Add track before cursor"
                       "  K       Move track up"
                       "  J       Move track down"
                       "  d/Del   Delete track"
                       "  n       New empty playlist"
                       "  b       Browse files to add"
                       "  o       Open M3U file"
                       "  w       Save/write playlist"
                       "  ?       This help"
                       "  q/Esc   Quit")))
    (draw-box bx by box-w box-h :title "Keybindings" :active-p t)
    (clear-panel-content bx by box-w box-h)
    (loop for line in help-lines
          for row from (1+ by)
          do (cursor-to row (+ bx 2))
             (fg :white)
             (princ (subseq line 0 (min (length line) (- box-w 4))) *terminal-io*))
    (reset)
    (force-output *terminal-io*)))

;;; ── Event loop ────────────────────────────────────────────────────

(defun handle-key (key)
  "Process a key event and dispatch to the appropriate action."
  ;; Clear transient message on any keypress
  (setf (app-message *app*) nil)

  ;; Help mode: any key dismisses
  (when (eq (app-mode *app*) :help)
    (setf (app-mode *app*) :normal)
    (full-render)
    (return-from handle-key))

  ;; Browse mode: file browser has its own keybindings
  (when (eq (app-mode *app*) :browse)
    (let ((code (key-event-code key))
          (char (key-event-char key)))
      (cond
        ;; Close browser
        ((or (eq code +key-escape+) (eql char #\q))
         (setf (app-mode *app*) :normal
               (app-browser *app*) nil)
         (full-render))
        ;; Navigation
        ((or (eq code +key-down+) (eql char #\j))  (browser-move-down)  (render-browser))
        ((or (eq code +key-up+) (eql char #\k))    (browser-move-up)    (render-browser))
        ((eq code +key-page-down+)                  (browser-page-down)  (render-browser))
        ((eq code +key-page-up+)                    (browser-page-up)    (render-browser))
        ;; Enter directory or add file
        ((eq code +key-enter+)
         (browser-enter)
         ;; If we added a file, re-render everything; if navigated dir, just browser
         (if (eq (app-mode *app*) :browse)
             (render-browser)
             (full-render)))
        ;; Backspace goes up a directory
        ((eq code +key-backspace+)
         (let* ((br (app-browser *app*))
                (parent (uiop:pathname-parent-directory-pathname
                         (browser-current-dir br))))
           (when (and parent (not (equal parent (browser-current-dir br))))
             (setf (browser-current-dir br) parent)
             (browser-refresh br)
             (render-browser))))
        ;; Space toggles selection
        ((eql char #\Space)
         (let ((br (app-browser *app*)))
           (browser-toggle-selection br)
           (browser-move-down)  ; move to next after selecting
           (render-browser)))
        ;; 'a' adds all selected files (or current if none selected)
        ((eql char #\a)
         (browser-add-all-selected)
         (if (eq (app-mode *app*) :browse)
             (render-browser)
             (full-render)))
        ;; 'r' toggles recursive mode
        ((eql char #\r)
         (let ((br (app-browser *app*)))
           (setf (browser-recursive-p br) (not (browser-recursive-p br)))
           (browser-refresh br)
           (render-browser)))))
    (return-from handle-key))

  (let ((code (key-event-code key))
        (char (key-event-char key))
        (ctrl (key-event-ctrl-p key)))
    (cond
      ;; Quit
      ((or (eq code +key-escape+)
           (and (eql char #\q) (not ctrl)))
       (setf (app-running-p *app*) nil)
       (return-from handle-key))

      ;; Help
      ((eql char #\?)
       (setf (app-mode *app*) :help)
       (show-help)
       (return-from handle-key))

      ;; Navigation
      ((or (eq code +key-down+)  (eql char #\j)) (move-cursor-down))
      ((or (eq code +key-up+)    (eql char #\k)) (move-cursor-up))
      ((eq code +key-page-down+)                  (page-down))
      ((eq code +key-page-up+)                    (page-up))
      ((eq code +key-home+)                       (move-cursor-top))
      ((eq code +key-end+)                        (move-cursor-bottom))
      ((eql char #\g)                             (move-cursor-top))
      ((eql char #\G)                             (move-cursor-bottom))

      ;; Switch pane
      ((eq code +key-tab+)
       (setf (app-active-pane *app*)
             (if (eq (app-active-pane *app*) :tracklist) :details :tracklist)))

      ;; Track operations
      ((eql char #\K) (move-track-up))
      ((eql char #\J) (move-track-down))
      ((or (eql char #\d) (eq code +key-delete+)) (delete-selected-track))
      ((eql char #\a) (add-track-at :after))
      ((eql char #\A) (add-track-at :before))

      ;; File browser
      ((eql char #\b) (open-browser)
       (full-render)
       (render-browser)
       (return-from handle-key))

      ;; Refresh track durations
      ((eql char #\R) (refresh-all-durations))

      ;; File operations
      ((eql char #\w) (save-playlist))
      ((eql char #\o) (open-playlist))

      ;; New playlist
      ((eql char #\n)
       (setf (app-playlist *app*)
             (make-playlist "Untitled"
                            :elements (list)))
       (setf (app-filepath *app*) nil)
       (let ((tl (current-tracklist)))
         (when tl
           (setf (tracklist-panel-cursor tl) 0
                 (tracklist-panel-scroll-offset tl) 0)))
       (sync-details)
       (setf (app-message *app*) "New playlist created"))

      ;; Edit playlist metadata
      ((eql char #\e)
       (edit-playlist-metadata)
       (return-from handle-key)))

    ;; Always re-render after a keypress
    (full-render)))

(defun event-loop ()
  "Main event loop: read keys, handle, render."
  (loop while (app-running-p *app*)
        do (let ((key (read-key)))
             (when key
               (handle-key key)))))

;;; ── Entry point ───────────────────────────────────────────────────

(defun run-tui (&optional filepath)
  "Launch the playlisp TUI editor.
   If FILEPATH is given, load the M3U playlist from that path."
  (let ((*app* (make-instance 'app-state)))
    ;; Load playlist if given
    (when filepath
      (setf (app-filepath *app*) (namestring (truename filepath)))
      (handler-case
          (setf (app-playlist *app*) (parse-m3u-file filepath))
        (error (e)
          (setf (app-playlist *app*)
                (make-playlist (file-namestring filepath))
                (app-message *app*)
                (format nil "Load error: ~A" e)))))
    ;; Default to empty playlist
    (unless (app-playlist *app*)
      (setf (app-playlist *app*)
            (make-playlist "Untitled"
                           :elements (list))))
    ;; Setup layout
    (with-raw-terminal
      (destructuring-bind (w h) (terminal-size)
        (let ((lay (make-layout)))
          (setf (app-layout *app*) lay)
          (layout-compute lay w h)))
      ;; Initial render
      (sync-details)
      (full-render)
      ;; Run
      (event-loop))
    ;; Exit SBCL when TUI closes
    (sb-ext:exit)))
