;;; m3u-operations.lisp - functions to maniuplate playlist objects

(uiop:define-package #:playlisp/m3u-operations
  (:use #:cl #:rutils)
  (:use-reexport #:playlisp/parser)
  (:nicknames :m3uop)
  (:export
   #:add-playlist-element
   #:get-audio-duration
   #:make-track-from-file))

(in-package :playlisp/m3u-operations)

;; ;;; create a new playlist

(defun read-playlist (playlist-path)
  "Read a playlist from PLAYLIST-PATH and return an instance of playlist/parser:PLAYLIST"
  (parse-m3u-file playlist-path))

;; ;;; add a track to a playlist

(defgeneric (setf add-playlist-element) (new-track playlist position)
  (:documentation "Setter for playlist-elements. 
Ex: (setf (add-playlist-element *playlist* 4) *track*)"))

(defmethod (setf add-playlist-element) ((new-track track) (playlist playlist) (index number))
  "given an instance of 'PLAYLIST, insert NEW-TRACK an instance of 'TRACK  at the
   INDEX of its elements slot.

   Called: (setf (add-playlist-element *playlist* 4) *track*) inserts
   *track* into the logical fourth position of the playlist-elements
   list."
  (let* ((elements (playlist-elements playlist))
         (index (1- index))) ; we have one-indexed the playlist
                                        ; elements, for silly humanz, but our
                                        ; lists still start at zero
    (cond ((= index -1) ;; end of list
           (setf (playlist-elements playlist) (append elements (list new-track))))
          ((= index 0) ;; front of list
           (setf (playlist-elements playlist) (append (list new-track) elements))
           ())
          (t
           (setf (playlist-elements playlist) (append (subseq elements 0 index)
                                                      (list new-track)
                                                      (nthcdr index elements)))))))

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

;; ;;; move a track within a playlist

;; ;;; delete a track from a playlist

;; ;;; reify a playlist into an m3u file for external consumption
