;; -*-lisp-*-

(uiop:define-package #:playlisp
  (:use #:cl
        #:parsector)
  (:export #:playlist
           #:playlist-name
           #:playlist-phase
           #:playlist-duration
           #:playlist-curator
           #:playlist-description
           #:playlist-elements
           #:make-playlist
           #:track
           #:title
           #:artist
           #:track-path
           #:playlist-position
           #:runtime
           #:make-track
           #:decode-m3u))

(in-package :playlisp)

(defclass playlist ()
  ((playlist-name
    :accessor playlist-name :initarg :playlist-name
    :initform (error "Playlist instances require a name."))
   (playlist-phase
    :accessor playlist-phase :initarg :playlist-phase
    :initform nil
    :documentation "The name of a 'phase' within a playlist, ususally denoting tone or
mood of the playlist segment")
   
   (playlist-duration
    :accessor playlist-duration :initarg :playlist-duration
    :initform 0
    :documentation "The runtime length of the entire playlist.")
   
   (playlist-curator
    :accessor playlist-curator :initarg :playlist-curator
    :documentation "A record of whose fault it is.")
   
   (playlist-description
    :accessor playlist-description :initarg :playlist-description
    :initform nil
    :documentation "A human readable description of the playlist's overall mood or tone of

the playlist.")
   (playlist-elements
    :accessor playlist-elements :initarg :playlist-elements
    :initform (list)
    :documentation "The list of tracks to queue for playback. Tracks are instances of the track class."))
  
  (:documentation "This class holds the components of an m3u style playlist. Initially
written for dynamic playlist support in the Asteroid Radio project."))

(defun make-playlist (play-list &key (phase nil) (duration nil)
                                  (curator (uiop:getenv "USER")) (description nil) (elements (list)))
  (make-instance 'playlist :playlist-name play-list
                           :playlist-phase phase
                           :playlist-duration duration
                           :playlist-curator curator
                           :playlist-description description
                           :playlist-elements elements))

(defclass track ()
  ((title :accessor title :initarg :title
          :initform nil
          :documentation "The title of the track.")
   (artist :accessor artist :initarg :artist
           :initform nil
           :documentation "The artist that created the track.")
   (track-path :accessor track-path :initarg
               :track-path :initform (error "at minimum, a track needs a path to the media asset.")
               :documentation "The physical path to the playback track asset.")
   (qnumber :accessor qnumber :initarg :qnumber :initform nil
            :documentation "The numeric position of the track within the represented playlist.")
   (runtime :accessor runtime :initarg :runtime :initform nil
            :documentation "The real-time length of this track.")))

(defun make-track (title path &key (artist nil) (runtime nil) (position nil))
  (make-instance 'track
                 :qnumber 
                 :title title
                 :artist artist
                 :track-path path
                 :playlist-position position
                 :runtime runtime))

;; ==================================================
;;  Parsing the playlist data
;; ==================================================

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

