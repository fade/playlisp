;;; playlisp/tests.lisp - Test suite for playlisp

(uiop:define-package #:playlisp/tests
  (:use #:cl #:parsector #:playlisp #:parachute)
  (:import-from #:playlisp/parser
                #:extm3u-header-parser
                #:metadata-line-parser
                #:extinf-line-parser
                #:path-line-parser
                #:empty-line-parser
                #:track-entry-parser
                #:playlist-line-parser
                #:m3u-parser
                #:parse-m3u-file)
  (:shadowing-import-from #:parsector #:skip #:fail))

(in-package #:playlisp/tests)

(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(defun capture-parse-error (parser string)
  (multiple-value-bind (always-nil err) (ignore-errors (parse-string parser string))
    (declare (ignore always-nil))
    err))

(define-test all-tests)

(define-test extm3u-header-parser-tests
  (is equal nil (parse-string (extm3u-header-parser) "#EXTM3U
"))
  (fail (parse-string (extm3u-header-parser) "NO_EXTM3U
") 'parser-error)
  (fail (parse-string (extm3u-header-parser) "#EXTM3U") 'parser-error))

(define-test metadata-line-parser-tests
  (is equal '(playlist-name . "My Awesome Mix") (parse-string (metadata-line-parser) "#PLAYLIST:My Awesome Mix
"))
  (is equal '(playlist-duration . "6 hours (approx)") (parse-string (metadata-line-parser) "#DURATION:6 hours (approx)
"))
  (fail (parse-string (metadata-line-parser) "NOT-METADATA:value
") 'parser-error)
  (fail (parse-string (metadata-line-parser) "#PLAYLIST
") 'parser-error))

(define-test extinf-line-parser-tests
  (is equal '(:DURATION 225 :TITLE "Artist - Song 1") (parse-string (extinf-line-parser) "#EXTINF:225,Artist - Song 1
"))
  (is equal '(:DURATION 180 :TITLE "Artist - Song 3") (parse-string (extinf-line-parser) "#EXTINF:180,Artist - Song 3
"))
  (is equal '(:DURATION -1 :TITLE "Artist - Song 2") (parse-string (extinf-line-parser) "#EXTINF:-1,Artist - Song 2
"))
  (fail (parse-string (extinf-line-parser) "#EXTINF:invalid,Title
") 'parser-error)
  (fail (parse-string (extinf-line-parser) "NOTEXTINF:180,Title
") 'parser-error))

(define-test path-line-parser-tests
  (is string= "/music/song1.mp3" (parse-string (path-line-parser) "/music/song1.mp3
"))
  (fail (parse-string (path-line-parser) "#EXTINF:180,Title
") 'parser-error))

(define-test empty-line-parser-tests
  (is equal nil (parse-string (empty-line-parser) "
"))
  (is equal nil (parse-string (empty-line-parser) "   
"))
  (fail (parse-string (empty-line-parser) "not empty
") 'parser-error))

(define-test track-entry-parser-tests
  (let ((track (parse-string (track-entry-parser) "#EXTINF:180,Artist - Song 3
/music/song3.mp3
")))
    (is eq 'track (type-of track))
    (is string= "Artist - Song 3" (title track))
    (is = 180 (runtime track))
    (is string= "/music/song3.mp3" (track-path track)))
  (fail (parse-string (track-entry-parser) "#EXTINF:180,Title
#EXTINF:another-line
") 'parser-error))

(define-test playlist-line-parser-tests
  (is equal '(playlist-name . "My Awesome Mix") (parse-string (playlist-line-parser) "#PLAYLIST:My Awesome Mix
"))
  (is equal nil (parse-string (playlist-line-parser) "   
"))
  (let ((track (parse-string (playlist-line-parser) "#EXTINF:180,Artist - Song 3
/music/song3.mp3
")))
    (is eq 'track (type-of track)))
  (fail (parse-string (playlist-line-parser) "NOT-A-PLAYLIST-LINE
") 'parser-error))

(define-test m3u-parser-integration-test
  (let* ((file-path "/home/fade/SourceCode/lisp/playlisp/playlists/underworld-and-friends.m3u")
         (playlist (parse-m3u-file file-path)))
    (is eq 'playlist (type-of playlist))
    (is string= "Underworld & Friends" (playlist-name playlist))
    (is string= "Underworld & Friends" (playlist-phase playlist))
    (is string= "6 hours (approx)" (playlist-duration playlist))
    (is string= "Asteroid Radio" (playlist-curator playlist))
    (is string= "Underworld and adjacent electronic artists - techno, electro, and progressive sounds" (playlist-description playlist))
    (is = 35 (length (playlist-elements playlist))) ; Check number of tracks
    (is string= "Underworld - Born Slippy (Nuxx)" (title (first (playlist-elements playlist))))
    (is = -1 (runtime (first (playlist-elements playlist))))
    (is string= "/app/music/Underworld - Second Toughest In The Infants (flac)/Second Toughest In The Infants (CD2)/01 Born Slippy (Nuxx).flac" (track-path (first (playlist-elements playlist))))))

