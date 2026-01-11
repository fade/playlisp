;; -*-lisp-*-
(defpackage :playlisp
  (:nicknames #:play)
  (:use :cl :parsnip)
  (:use :playlisp.app-utils)
  (:export :-main))

(in-package :playlisp)

#|
The Goal here is to read m3u files and turn them into something we can compute about, then reserialize to m3u files.

An m3u file has only a /de facto/ structure, bu it roughly looks like this:

#EXTM3U
#PLAYLIST:Evening Descent
#PHASE:Evening Descent
#DURATION:6 hours (approx)
#CURATOR:Asteroid Radio
#DESCRIPTION:Winding down transitional ambient for the evening hours (18:00-00:00)

#EXTINF:-1,Brian Eno - Dust Shuffle
/app/music/Brian Eno/2011 - Small Craft On a Milk Sea/B4 Dust Shuffle.flac
#EXTINF:-1,Biosphere - Black Mesa
/app/music/Biosphere - The Petrified Forest (2017) - CD FLAC/02. Biosphere - Black Mesa.flac
#EXTINF:-1,Labradford - C
|#

(defclass playlist ()
  ((playlist-name :accessor playlist-name :initarg :playlist-name
                  :initform (error "Playlist instances require a name."))
   (playlist-phase :accessor playlist-phase :initarg :playlist-phase
                   :initform nil
                   :documentation "The name of a 'phase' within a playlist, ususally denoting tone or
mood of the playlist segment")
   (playlist-duration :accessor playlist-duration :initarg :playlist-duration
                      :initform 0
                      :documentation "The runtime length of the entire playlist.")
   (playlist-curator :accessor playlist-curator :initarg :playlist-curator
                     :documentation "A record of whose fault it is.")
   (playlist-description :accessor playlist-description :initarg :playlist-description
                         :initform nil
                         :documentation "A human readable description of the playlist's overall mood or tone of
the playlist.")
   (playlist-elements :accessor playlist-elements :initarg :playlist-elements
                      :initform (list)
                      :documentation "The list of tracks to queue for playback. Tracks are instances of the track class."))
  (:documentation "This class holds the components of an m3u style playlist. Initially written for dynamic playlist support in the Asteroid Radio project."))

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
   (playlist-position :accessor playlist-position :initarg :playlist-position :initform nil
                      :documentation "The numerical position of this instance in an ordered playlist.")
   (runtime :accessor runtime :initarg :runtime :initform nil
            :documentation "The real-time length of this track.")))

(defun make-track (title path &key (artist nil) (runtime nil) (position nil))
  (make-instance 'track :title title
                        :artist artist
                        :track-path path
                        :playlist-position position
                        :runtime runtime))

;; ==================================================
;;  Parsing the playlist data
;; ==================================================



(defun decode-m3u (m3udata)
  (let ((header nil)
        (tracks nil))
    (ignorable header tracks)
    (loop for line in m3udata
          do (format t "~&~A" line))))

(defun read-playlist (playlist)
  (let* ((m3ufile (rutils:slurp playlist)))
    (values m3ufile)))

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

