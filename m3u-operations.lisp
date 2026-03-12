;;; m3u-operations.lisp - functions to maniuplate playlist objects

(uiop:define-package #:playlisp/m3u-operations
  (:use #:cl #:alexandria)
  (:local-nicknames (:a :alexandria))
  (:use-reexport #:playlisp/parser)
  (:nicknames :m3uop)
  (:export
   #:add-playlist-element))

(in-package :playlisp/m3u-operations)

;;; create a new playlist

(defun read-playlist (playlist-path)
  "Read a playlist from PLAYLIST-PATH and return an instance of playlist/parser:PLAYLIST"
  (parse-m3u-file playlist-path))

;; (defun make-playlist (pname phase &key (curator (uiop:getenv "USER"))
;;                                     (description "Every one a description requires.") (elements nil))
;;   "Create an empty instance of the 'PLAYLIST class."
;;   (make-instance 'playlist
;;                  :playlist-name pname
;;                  :playlist-phase phase
;;                  :playlist-curator curator
;;                  :playlist-description description
;;                  :playlist-elements elements))

;; TODO: future local override - when needed, shadow make-playlist here with:
;; (defun make-playlist (pname &key (phase nil) (duration nil)
;;                               (curator (uiop:getenv "USER"))
;;                               (description nil) (elements (list)))
;;   "Create an empty PLAYLIST instance (m3u-operations variant)."
;;   (make-instance 'playlist
;;                  :playlist-name pname
;;                  :playlist-phase phase
;;                  :playlist-duration duration
;;                  :playlist-curator curator
;;                  :playlist-description description
;;                  :playlist-elements elements))


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

;;; move a track within a playlist

(defgeneric rmtrack (track playlist)
  (:documentation "Remove a track from the playlist-elements list in a playlist object"))

(defmethod rmtrack ((track track) (playlist playlist))
  (let* ((elements (playlist-elements playlist))
         (tname (track-title track))
         (tartist (track-artist track))
         (index (qnumber track)))
    ))


;;; delete a track from a playlist

;;; reify a playlist into an m3u file for external consumption
