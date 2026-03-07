;;; m3u-operations.lisp - functions to maniuplate playlist objects

(uiop:define-package #:playlisp/m3u-operations
  (:use #:cl #:rutils)
  (:use-reexport #:playlisp/parser)
  (:nicknames :m3uop)
  (:export
   #:add-playlist-element))

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

;; ;;; move a track within a playlist

;; ;;; delete a track from a playlist

;; ;;; reify a playlist into an m3u file for external consumption
