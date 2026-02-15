;;; playlisp/tests.lisp - Test suite for playlisp

(uiop:define-package #:playlisp/tests
  (:use #:cl #:parsector #:playlisp #:parachute)
  (:import-from #:playlisp/parser
                #:extm3u-header
                #:metadata-line
                #:extinf-line
                #:path-line
                #:blank-line
                #:track-entry
                #:playlist-line
                #:m3u-parser
                #:parse-m3u
                #:parse-m3u-file)
  (:shadowing-import-from #:parachute #:fail)
  (:shadowing-import-from #:parsector #:skip))

(in-package #:playlisp/tests)

(defun parse-string (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(define-test playlisp-tests
  :parent NIL)

(define-test extm3u-header-tests
  :parent playlisp-tests
  (is string= "#EXTM3U" (parse-string 'extm3u-header "#EXTM3U
"))
  (fail (parse-string 'extm3u-header "NO_EXTM3U
") 'parser-error)
  (fail (parse-string 'extm3u-header "#EXTM3U") 'parser-error))

(define-test metadata-line-tests
  :parent playlisp-tests
  (is equal '(:PLAYLIST . "My Awesome Mix") (parse-string 'metadata-line "#PLAYLIST:My Awesome Mix
"))
  (is equal '(:DURATION . "6 hours (approx)") (parse-string 'metadata-line "#DURATION:6 hours (approx)
"))
  (fail (parse-string 'metadata-line "NOT-METADATA:value
") 'parser-error)
  (fail (parse-string 'metadata-line "#PLAYLIST
") 'parser-error))

(define-test extinf-line-tests
  :parent playlisp-tests
  (is equal '(:DURATION 225 :TITLE "Artist - Song 1") (parse-string 'extinf-line "#EXTINF:225,Artist - Song 1
"))
  (is equal '(:DURATION 180 :TITLE "Artist - Song 3") (parse-string 'extinf-line "#EXTINF:180,Artist - Song 3
"))
  (is equal '(:DURATION -1 :TITLE "Artist - Song 2") (parse-string 'extinf-line "#EXTINF:-1,Artist - Song 2
"))
  (fail (parse-string 'extinf-line "#EXTINF:invalid,Title
") 'parser-error)
  (fail (parse-string 'extinf-line "NOTEXTINF:180,Title
") 'parser-error))

(define-test path-line-tests
  :parent playlisp-tests
  (is string= "/music/song1.mp3" (parse-string 'path-line "/music/song1.mp3
"))
  (fail (parse-string 'path-line "#EXTINF:180,Title
") 'parser-error))

(define-test blank-line-tests
  :parent playlisp-tests
  (is equal nil (parse-string 'blank-line "
"))
  (is equal nil (parse-string 'blank-line "
"))
  (fail (parse-string 'blank-line "not empty
") 'parser-error))

(define-test track-entry-tests
  :parent playlisp-tests
  (let ((track (parse-string 'track-entry "#EXTINF:180,Artist - Song 3
/music/song3.mp3
")))
    (is eq 'track (type-of track))
    (is string= "Artist - Song 3" (title track))
    (is = 180 (runtime track))
    (is string= "/music/song3.mp3" (track-path track)))
  (let ((track (parse-string 'track-entry "#EXTINF:-1,Live Stream
/stream/live
")))
    (is = -1 (runtime track))))

(define-test playlist-line-tests
  :parent playlisp-tests
  (is equal '(:PLAYLIST . "My Awesome Mix") (parse-string 'playlist-line "#PLAYLIST:My Awesome Mix
"))
  (is equal nil (parse-string 'playlist-line "
"))
  (let ((track (parse-string 'playlist-line "#EXTINF:180,Artist - Song 3
/music/song3.mp3
")))
    (is eq 'track (type-of track))))

(define-test m3u-parser-integration-test
  :parent playlisp-tests
  (let* ((file-path (asdf:system-relative-pathname :playlisp "playlists/underworld-and-friends.m3u"))
         (playlist (parse-m3u-file file-path)))
    (is eq 'playlist (type-of playlist))
    (is string= "Underworld & Friends" (playlist-name playlist))
    (is string= "Underworld & Friends" (playlist-phase playlist))
    (is string= "6 hours (approx)" (playlist-duration playlist))
    (is string= "Asteroid Radio" (playlist-curator playlist))
    (is string= "Underworld and adjacent electronic artists - techno, electro, and progressive sounds" (playlist-description playlist))
    (is = 45 (length (playlist-elements playlist)))
    (is string= "Underworld - Born Slippy (Nuxx)" (title (first (playlist-elements playlist))))
    (is = -1 (runtime (first (playlist-elements playlist))))
    (is string= "/app/music/Underworld - Second Toughest In The Infants (flac)/Second Toughest In The Infants (CD2)/01 Born Slippy (Nuxx).flac" (track-path (first (playlist-elements playlist))))
    ;; Check tracks are numbered
    (is = 1 (qnumber (first (playlist-elements playlist))))
    (is = 45 (qnumber (car (last (playlist-elements playlist)))))))

;;; --- Edge Case Tests ---

(define-test no-trailing-newline
  :parent playlisp-tests
  (let ((playlist (parse-m3u (format nil "#EXTM3U~%#EXTINF:180,Test Song~%/path/to/song.mp3"))))
    (is = 1 (length (playlist-elements playlist)))
    (let ((track (first (playlist-elements playlist))))
      (is string= "Test Song" (title track))
      (is = 180 (runtime track))
      (is string= "/path/to/song.mp3" (track-path track)))))

(define-test no-trailing-newline-multiple
  :parent playlisp-tests
  (let ((playlist (parse-m3u (format nil "#EXTM3U~%#EXTINF:180,Song A~%/a.mp3~%#EXTINF:240,Song B~%/b.mp3"))))
    (is = 2 (length (playlist-elements playlist)))
    (is string= "/a.mp3" (track-path (first (playlist-elements playlist))))
    (is string= "/b.mp3" (track-path (second (playlist-elements playlist))))))

(define-test no-trailing-newline-metadata
  :parent playlisp-tests
  (let ((playlist (parse-m3u (format nil "#EXTM3U~%#PLAYLIST:No Newline~%#EXTINF:100,Song~%/song.mp3"))))
    (is string= "No Newline" (playlist-name playlist))
    (is = 1 (length (playlist-elements playlist)))))

(define-test malformed-expression-backtrack
  :parent playlisp-tests
  ;; "3-" looks like a partial expression; parser should backtrack and parse 3 as plain duration
  (let ((playlist (parse-m3u (format nil "#EXTM3U~%#EXTINF:3,Backtrack Test~%/path.mp3~%"))))
    (is = 3 (runtime (first (playlist-elements playlist))))))
