;;; m3u-operations.lisp - functions to maniuplate playlist objects

(uiop:define-package #:playlisp/m3u-operations
  (:use #:cl #:alexandria)
  (:use-reexport #:playlisp/parser)
  (:nicknames :m3uop)
  (:export
   #:add-playlist-element
   #:find-track
   #:rmtrack
   #:get-audio-duration
   #:make-track-from-file))

(in-package :playlisp/m3u-operations)

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
  "set the qnumber for a list of track objects to reflect is position in the playlist."
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
      (let* ((cmd (format nil "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ~S"
                          (namestring filepath)))
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

(defgeneric rmtrack (track playlist)
  (:documentation "Remove a track from the playlist-elements list in a playlist object"))

(defmethod rmtrack ((track track) (playlist playlist))
  "Remove TRACK from PLAYLIST by identity, then renumber the remaining tracks."
  (setf (playlist-elements playlist)
        (resmoother (remove track (playlist-elements playlist) :test #'eq)))
  playlist)

;;; TODO reify a playlist into an m3u file for external consumption
