;;; m3u-operations.lisp - functions to maniuplate playlist objects

(uiop:define-package #:playlisp/m3u-operations
  (:use #:cl #:rutils)
  (:use-reexport #:playlisp/parser)
  (:nicknames :m3uop))

(in-package :playlisp/m3u-operations)

;; ;;; create a new playlist

(defun read-playlist (playlist-path)
  "Read a playlist from PLAYLIST-PATH and return an instance of 'playlist/parser:PLAYLIST"
  (playlisp:parse-m3u-file playlist-path))

;; ;;; add a track to a playlist

;; (defgeneric (setf add-playlist-element) (new-track playlist)
;;   (:documentation "Setter for playlist-elements"))

;; (defmethod (setf add-playlist-element) (new-track (playlist playlist/parser:playlist))
;;   (setf (append (playlist-elements playlist) (list new-track))))

;; ;;; move a track within a playlist

;; ;;; delete a track from a playlist

;; ;;; reify a playlist into an m3u file for external consumption
