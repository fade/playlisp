;;; parser.lisp - M3U Playlist Parser for playlisp

(uiop:define-package #:playlisp/parser
  (:use #:cl #:parsector #:playlisp)
  (:export #:parse-m3u
           #:parse-m3u-file
           #:extm3u-header
           #:metadata-line
           #:extinf-line
           #:path-line
           #:track-entry
           #:blank-line
           #:playlist-line
           #:m3u-parser))

(in-package #:playlisp/parser)

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

(defun parse-m3u-file (pathname)
  "Parses an M3U playlist file into a PLAYLIST instance."
  (with-open-file (stream pathname :direction :input)
    (parse 'm3u-parser stream)))
