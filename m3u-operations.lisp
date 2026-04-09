;;; m3u-operations.lisp - functions to maniuplate playlist objects

(uiop:define-package #:playlisp/m3u-operations
  (:use #:cl #:alexandria)
  (:local-nicknames (:a :alexandria))
  (:import-from #:parse-number #:parse-number)
  (:use-reexport #:playlisp/parser)
  (:nicknames :m3uop)
  (:export
   #:add-playlist-element
   #:find-track
   #:get-audio-duration
   #:make-track-from-file
   #:write-m3u-file
   #:move-track-up
   #:move-track-down
   #:delete-track
   #:*media-host-root*
   #:*media-target-root*
   #:configure-media-roots
   #:host->target-path
   #:target->host-path
   #:normalize-playlist-paths))

(in-package :playlisp/m3u-operations)

;;; ── Media root rewriting ────────────────────────────────────────
;;;
;;; playlisp is used as both an editing tool on a developer workstation
;;; and as a library that feeds playlists to services running elsewhere
;;; (for example, an Icecast/Liquidsoap rig running in a container that
;;; mounts the music library at a different path).  Track objects carry
;;; the *host* path in their track-path slot -- the path that works on
;;; the machine actually reading the files -- and on serialisation the
;;; host prefix is swapped for the target (container) prefix.  Both
;;; roots are tunable so the same code can serve arbitrary deployments.

(defvar *media-host-root* nil
  "Directory prefix that track-path slots are expected to live under on
the machine running playlisp.  When NIL, no rewriting is performed and
paths are written to m3u files verbatim.  Trailing separator is
normalised by CONFIGURE-MEDIA-ROOTS.")

(defvar *media-target-root* nil
  "Directory prefix that should replace *MEDIA-HOST-ROOT* when a
playlist is serialised to an m3u file.  When NIL, no rewriting is
performed.  Typically this is the path at which the host media tree is
mounted inside the consuming environment (e.g. \"/app/music/\" in a
container).")

(defun %normalise-root (root)
  "Return ROOT as a string that ends in a single #\\/ separator, or NIL
if ROOT is NIL."
  (when root
    (let ((s (etypecase root
               (string root)
               (pathname (namestring root)))))
      (if (and (plusp (length s))
               (char= (char s (1- (length s))) #\/))
          s
          (concatenate 'string s "/")))))

(defun configure-media-roots (&key host target)
  "Set *MEDIA-HOST-ROOT* and *MEDIA-TARGET-ROOT* for the current
session.  Either keyword may be omitted, in which case the
corresponding root is left unchanged; passing an explicit NIL clears
it.  Trailing separators are normalised so callers may supply either
\"/foo\" or \"/foo/\"."
  (when host
    (setf *media-host-root* (%normalise-root host)))
  (when target
    (setf *media-target-root* (%normalise-root target)))
  (values *media-host-root* *media-target-root*))

(defun %coerce-path-string (path)
  (etypecase path
    (string path)
    (pathname (namestring path))
    (null "")))

(defun host->target-path (path)
  "Return PATH rewritten so it is expressed relative to
*MEDIA-TARGET-ROOT*.

Rules, in order:
  * If either root is unset, PATH is returned unchanged.
  * If PATH already begins with *MEDIA-TARGET-ROOT*, it is returned
    unchanged (idempotent; safe to call on already-target paths).
  * If PATH begins with *MEDIA-HOST-ROOT*, that prefix is replaced with
    *MEDIA-TARGET-ROOT*.
  * Otherwise a warning is signalled and PATH is returned as-is so
    stray absolute paths do not crash serialisation."
  (let ((path-str (%coerce-path-string path)))
    (cond
      ((or (null *media-host-root*)
           (null *media-target-root*))
       path-str)
      ((alexandria:starts-with-subseq *media-target-root* path-str)
       path-str)
      ((alexandria:starts-with-subseq *media-host-root* path-str)
       (concatenate 'string
                    *media-target-root*
                    (subseq path-str (length *media-host-root*))))
      (t
       (warn "host->target-path: ~S lives under neither *media-host-root* ~S nor *media-target-root* ~S; passing through."
             path-str *media-host-root* *media-target-root*)
       path-str))))

(defun target->host-path (path)
  "Return PATH rewritten so it is expressed relative to
*MEDIA-HOST-ROOT*.  Inverse of HOST->TARGET-PATH and likewise
idempotent: calling it on a path that is already host-relative returns
the path unchanged.  Stray paths that live under neither root are
returned with a warning."
  (let ((path-str (%coerce-path-string path)))
    (cond
      ((or (null *media-host-root*)
           (null *media-target-root*))
       path-str)
      ((alexandria:starts-with-subseq *media-host-root* path-str)
       path-str)
      ((alexandria:starts-with-subseq *media-target-root* path-str)
       (concatenate 'string
                    *media-host-root*
                    (subseq path-str (length *media-target-root*))))
      (t
       (warn "target->host-path: ~S lives under neither *media-host-root* ~S nor *media-target-root* ~S; passing through."
             path-str *media-host-root* *media-target-root*)
       path-str))))

(defun normalize-playlist-paths (playlist &key (to :host))
  "Rewrite every track-path in PLAYLIST so it is expressed relative to
the requested root.  TO is :HOST (default) or :TARGET.  Tracks already
on the requested side of the rewrite are left alone; tracks on the
other side are converted; stray paths trigger a warning and are left
unchanged.  A playlist containing a mixture of host-rooted and
target-rooted paths is therefore normalised in a single pass.  The
playlist is mutated and returned."
  (let ((rewriter (ecase to
                    (:host   #'target->host-path)
                    (:target #'host->target-path))))
    (dolist (track (playlist-elements playlist))
      (setf (track-path track) (funcall rewriter (track-path track)))))
  playlist)

;;; create a new playlist

(defun read-playlist (playlist-path)
  "Read a playlist from PLAYLIST-PATH and return an instance of playlist/parser:PLAYLIST"
  (parse-m3u-file playlist-path))

;;; playlist element sanitizers


;;; add a track to a playlist, in whatever position.. begin, end, middle.

(defgeneric (setf add-playlist-element) (new-track playlist position)
  (:documentation "Setter for playlist-elements. 
Ex: (setf (add-playlist-element *playlist* 4) *track*)"))

(defun resmoother (list-of-tracks)
  "set the qnumber for a list of track objects to reflect its position in the playlist."
  (loop for track in list-of-tracks
        for index from 1
        do (setf (qnumber track) index)
        finally (return list-of-tracks)))

(defmethod (setf add-playlist-element) ((new-track track) (playlist playlist) (index number))
  "given an instance of 'PLAYLIST, insert NEW-TRACK an instance of 'TRACK  at the
   INDEX of its elements slot.

   Called: (setf (add-playlist-element *playlist* 4) *track*) inserts
   *track* into the logical fourth position of the playlist-elements
   list."
  (let* ((elements (playlist-elements playlist))
         (index (if (>= index 0) ;; if it's not negative, normalize it. Otherwise just return it.
                    (1- index)
                    index)))
    (cond ((= index -1)
           (setf (playlist-elements playlist) (append elements (list new-track))))
          ((= index 0) ;; front of list
           (setf (playlist-elements playlist) (append (list new-track) elements)))
          (t
           (setf (playlist-elements playlist) (append (subseq elements 0 index)
                                                      (list new-track)
                                                      (nthcdr index elements)))))))

(defmethod (setf add-playlist-element) :after ((new-track track) (playlist playlist) (index number))
  "after all the setting is done, zip down the list setting each track's
qnumber relative to its position in the list."
  (setf (playlist-elements playlist)
        (resmoother (playlist-elements playlist))))

;;; ── Audio file metadata ───────────────────────────────────────────

(defun get-audio-duration (filepath)
  "Get duration in seconds from an audio file using ffprobe. Returns NIL on error."
  (handler-case
      (let* ((path-str (etypecase filepath
                         (string filepath)
                         (pathname (sb-ext:native-namestring filepath))))
             (cmd (format nil "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ~S"
                          path-str))
             (output (string-trim '(#\Space #\Newline #\Return)
                                  (uiop:run-program cmd :output :string :ignore-error-status t))))
        (when (and output (> (length output) 0))
          (let ((duration (ignore-errors (parse-number:parse-number output))))
            (when (and duration (numberp duration))
              (round duration)))))
    (error () nil)))

(defun make-track-from-file (filepath &key title)
  "Create a track from an audio file, reading duration via ffprobe.
   If TITLE is not provided, uses the filename."
  (let ((duration (get-audio-duration filepath))
        (name (or title (file-namestring filepath))))
    (make-track name filepath :runtime (or duration -1))))

;;; find a track within a playlist

(defun find-track (playlist key &key (by :title))
  "Find the first track in PLAYLIST whose field designated by BY matches KEY.
BY may be :title, :artist, or :qnumber.
String comparisons are case-insensitive. Returns the matching track or NIL."
  (find key (playlist-elements playlist)
        :key (ecase by
               (:title   #'title)
               (:artist  #'artist)
               (:qnumber #'qnumber))
        :test (if (eq by :qnumber)
                  #'eql
                  (lambda (k field) (string-equal k (or field ""))))))

;;; remove a track from a playlist

(defgeneric delete-track (track playlist)
  (:documentation "Remove a track from the playlist-elements list in a playlist object"))

(defmethod delete-track ((track track) (playlist playlist))
  "Remove TRACK from PLAYLIST by identity, then renumber the remaining tracks."
  (setf (playlist-elements playlist)
        (resmoother (remove track (playlist-elements playlist) :test #'eq)))
  playlist)

(defmethod delete-track ((index number) (playlist playlist))
  "Remove track at INDEX from PLAYLIST. Returns new cursor index."
  (let ((tracks (playlist-elements playlist)))
    (when (and tracks (< index (length tracks)))
      (setf (playlist-elements playlist)
            (resmoother (append (subseq tracks 0 index)
                                (nthcdr (1+ index) tracks)))))
    (min index (max 0 (1- (length (playlist-elements playlist)))))))

;;; ── Track reordering and deletion ───────────────────────────────

(defun move-track-up (playlist index)
  "Swap track at INDEX with the one above it. Returns new index, or INDEX if at top."
  (let ((tracks (playlist-elements playlist)))
    (when (and tracks (> index 0) (< index (length tracks)))
      (rotatef (nth index tracks) (nth (1- index) tracks))
      (setf (playlist-elements playlist) (resmoother tracks))
      (return-from move-track-up (1- index))))
  index)

(defun move-track-down (playlist index)
  "Swap track at INDEX with the one below it. Returns new index, or INDEX if at bottom."
  (let ((tracks (playlist-elements playlist)))
    (when (and tracks (< index (1- (length tracks))))
      (rotatef (nth index tracks) (nth (1+ index) tracks))
      (setf (playlist-elements playlist) (resmoother tracks))
      (return-from move-track-down (1+ index))))
  index)

;; (defun delete-track (playlist index)
;;   "Remove track at INDEX from PLAYLIST. Returns new cursor index."
;;   (let ((tracks (playlist-elements playlist)))
;;     (when (and tracks (< index (length tracks)))
;;       (setf (playlist-elements playlist)
;;             (resmoother (append (subseq tracks 0 index)
;;                                 (nthcdr (1+ index) tracks)))))
;;     (min index (max 0 (1- (length (playlist-elements playlist)))))))

;;; ── Write playlist to M3U file ──────────────────────────────────

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
      (format out "~A~%" (host->target-path (or (track-path track) ""))))))
