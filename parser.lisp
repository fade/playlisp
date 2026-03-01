;;; parser.lisp - M3U Playlist Parser for playlisp

(uiop:define-package #:playlisp/parser
  (:use #:cl #:parsector #:auto-text)
  (:export #:parse-m3u
           #:parse-m3u-file
           #:extm3u-header
           #:metadata-line
           #:extinf-line
           #:path-line
           #:track-entry
           #:blank-line
           #:playlist-line
           #:m3u-parser
           #:playlist
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
           #:qnumber
           #:runtime
           #:make-track))

(in-package #:playlisp/parser)

;;; --- Parser Objects and constructors ---

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
                 :qnumber position
                 :title title
                 :artist artist
                 :track-path path
                 :runtime runtime))

;;; --- Utility Parsers ---

(defun whitespace ()
  "Parse zero or more spaces or tabs (not newlines)."
  (skip-many (char-in #(#\Space #\Tab))))

(defun end-of-line-or-eof ()
  "Match end-of-line or end-of-file."
  (or! (end-of-line) (eof)))

(defparser comment-line ()
  (prog2! (char-of #\;)
          (let! ((chars (many-till (any-char) (lookahead (end-of-line-or-eof)))))
            (ok (coerce chars 'string)))
          (end-of-line)))

;;; --- Arithmetic Expression Parsers (for EXTINF duration) ---

(defun parens (parser)
  "Parse content surrounded by parentheses with optional whitespace."
  (between (progn! (char-of #\() (whitespace))
           parser
           (progn! (whitespace) (char-of #\)))))

(defparser number-expr ()
  (let! ((_ (whitespace))
         (num (natural))
         (_ (whitespace)))
    (ok num)))

;; Operator parsers - parse the operator character and return the function
(defun add-op ()
  (progn! (char-of #\+) (whitespace) (ok #'+)))

(defun sub-op ()
  (progn! (char-of #\-) (whitespace) (ok #'-)))

(defun mul-op ()
  (progn! (char-of #\*) (whitespace) (ok #'*)))

(defun div-op ()
  (progn! (char-of #\/) (whitespace) (ok #'/)))

;; `term` handles multiplication and division (higher precedence)
(defparser term-expr ()
  (chainl1 (or! (parens 'expr-parser)
                'number-expr)
           (or! (mul-op) (div-op))))

;; `expr` handles addition and subtraction (lower precedence)
(defparser expr-parser ()
  (chainl1 'term-expr
           (or! (add-op) (sub-op))))

;;; --- M3U Specific Parsers ---

(defparser extm3u-header ()
  (prog1!
   (string-of "#EXTM3U")
   (end-of-line)))

;; Parse a metadata key: uppercase letters, optionally with underscores/hyphens.
(defparser metadata-key ()
  (let! ((chars (collect1 (char-if (lambda (c)
                                     (or (upper-case-p c)
                                         (char= c #\_)
                                         (char= c #\-)))))))
    (ok (intern (coerce chars 'string) :keyword))))

;; Parse a metadata line like #KEY:value (e.g., #PLAYLIST:, #DURATION:, #CURATOR:).
(defparser metadata-line ()
  (let! ((_ (char-of #\#))
         (_ (not-followed-by (string-of "EXTINF")))
         (_ (not-followed-by (string-of "EXTM3U")))
         (key 'metadata-key)
         (_ (char-of #\:))
         (chars (many-till (any-char) (lookahead (end-of-line-or-eof)))))
    (ok (cons key (coerce chars 'string)))))

(defparser extinf-line ()
  (let! ((_ (string-of "#EXTINF:"))
         (duration (or! (try! 'expr-parser)
                        (let! ((_ (char-of #\-)) (n (natural)))
                          (ok (- n)))))
         (_ (char-of #\,))
         (chars (many-till (any-char) (lookahead (end-of-line-or-eof)))))
    (ok (list :duration duration :title (coerce chars 'string)))))

(defparser path-line ()
  (let! ((_ (not-followed-by (char-of #\#)))
         (chars (many-till (any-char) (lookahead (end-of-line-or-eof)))))
    (ok (coerce chars 'string))))

(defparser track-entry ()
  (let! ((inf 'extinf-line)
         (_ (end-of-line))
         (path (optional 'path-line)))
    (ok (make-instance 'track
                       :title (getf inf :title)
                       :runtime (getf inf :duration)
                       :track-path (or path "")))))

;; Parse a blank line (only whitespace before end-of-line).
(defparser blank-line ()
  (prog1! (whitespace) (lookahead (end-of-line-or-eof)) (ok :blank)))

(defparser playlist-line ()
  (choice (list (try! 'track-entry)
                (try! 'metadata-line)
                'comment-line
                'blank-line)))

(defparser m3u-parser ()
  (let! ((_ 'extm3u-header)
         (lines (many-till (let! ((line 'playlist-line)
                                  (_ (optional (end-of-line))))
                             (ok line))
                           (eof))))
    (let ((metadata nil)
          (tracks nil))
      (dolist (line lines)
        (cond
          ((consp line)
           (push line metadata))
          ((typep line 'track)
           (push line tracks))))
      (setf metadata (nreverse metadata))
      (setf tracks (nreverse tracks))
      ;; Number tracks by position
      (loop for track in tracks
            for i from 1
            do (setf (qnumber track) i))
      (let ((name (or (cdr (assoc :PLAYLIST metadata)) "Untitled"))
            (phase (cdr (assoc :PHASE metadata)))
            (duration (cdr (assoc :DURATION metadata)))
            (curator (cdr (assoc :CURATOR metadata)))
            (description (cdr (assoc :DESCRIPTION metadata))))
        (ok (make-playlist name
                           :phase phase
                           :duration duration
                           :curator curator
                           :description description
                           :elements tracks))))))

;;; --- Toplevel Function ---

(defun parse-m3u (string)
  "Parses an M3U playlist string into a PLAYLIST instance."
  (parse 'm3u-parser (make-string-input-stream string)))

(defun parse-m3u-file (pathname &key (encoding :utf8))
  "Parses an M3U playlist file into a PLAYLIST instance."
  (let* ((file-path pathname)
         (analysis (auto-text:analyze file-path :silent t))
         (encoding (or (getf analysis :encoding) encoding)))
    (with-open-file (stream pathname :direction :input :external-format encoding)
      (parse 'm3u-parser stream))))
