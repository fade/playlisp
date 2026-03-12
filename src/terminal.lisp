;;; src/terminal.lisp - Raw terminal mode and keyboard input for playlisp TUI
;;; Handles raw mode, alternate screen, terminal size, and key event parsing

(uiop:define-package #:playlisp/src/terminal
  (:use #:cl #:playlisp/src/ansi)
  (:export ;; Key events
           #:key-event
           #:make-key-event
           #:key-event-char
           #:key-event-code
           #:key-event-ctrl-p
           #:key-event-alt-p
           ;; Key codes
           #:+key-up+ #:+key-down+ #:+key-left+ #:+key-right+
           #:+key-enter+ #:+key-escape+ #:+key-tab+
           #:+key-backspace+ #:+key-delete+
           #:+key-home+ #:+key-end+
           #:+key-page-up+ #:+key-page-down+
           ;; Terminal control
           #:terminal-size
           #:enter-alternate-screen
           #:leave-alternate-screen
           #:with-raw-terminal
           #:read-key
           #:read-key-with-timeout))

(in-package #:playlisp/src/terminal)

(require :sb-posix)

;;; ── Key event class ───────────────────────────────────────────────

(defclass key-event ()
  ((char   :initarg :char   :accessor key-event-char   :initform nil)
   (code   :initarg :code   :accessor key-event-code   :initform nil)
   (ctrl-p :initarg :ctrl-p :accessor key-event-ctrl-p :initform nil)
   (alt-p  :initarg :alt-p  :accessor key-event-alt-p  :initform nil))
  (:documentation "Represents a keyboard input event"))

(defmethod print-object ((key key-event) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~@[char=~S~]~@[ code=~S~]~@[ ctrl~]~@[ alt~]"
            (key-event-char key) (key-event-code key)
            (key-event-ctrl-p key) (key-event-alt-p key))))

(defun make-key-event (&key char code ctrl-p alt-p)
  (make-instance 'key-event :char char :code code :ctrl-p ctrl-p :alt-p alt-p))

;;; ── Key codes ─────────────────────────────────────────────────────

(defconstant +key-up+        :up)
(defconstant +key-down+      :down)
(defconstant +key-left+      :left)
(defconstant +key-right+     :right)
(defconstant +key-enter+     :enter)
(defconstant +key-escape+    :escape)
(defconstant +key-tab+       :tab)
(defconstant +key-backspace+ :backspace)
(defconstant +key-delete+    :delete)
(defconstant +key-home+      :home)
(defconstant +key-end+       :end)
(defconstant +key-page-up+   :page-up)
(defconstant +key-page-down+ :page-down)

;;; ── Terminal mode ─────────────────────────────────────────────────

(defvar *original-termios* nil
  "Saved termios settings for restoring on exit.")

(defvar *raw-mode-p* nil)

(defvar *tty-stream* nil
  "Non-blocking stream to /dev/tty for input.")

(defvar *escape-timeout* 0.02
  "Seconds to wait for escape sequence bytes.")

(defun enable-raw-mode ()
  "Put terminal into raw mode for character-at-a-time input."
  (unless *raw-mode-p*
    (handler-case
        (let* ((fd (sb-sys:fd-stream-fd sb-sys:*stdin*))
               (orig (sb-posix:tcgetattr fd))
               (raw  (sb-posix:tcgetattr fd)))
          (setf *original-termios* orig)
          (setf (sb-posix:termios-iflag raw)
                (logand (sb-posix:termios-iflag raw)
                        (lognot (logior sb-posix:brkint sb-posix:icrnl
                                        sb-posix:inpck sb-posix:istrip
                                        sb-posix:ixon))))
          (setf (sb-posix:termios-oflag raw)
                (logand (sb-posix:termios-oflag raw)
                        (lognot sb-posix:opost)))
          (setf (sb-posix:termios-cflag raw)
                (logior (sb-posix:termios-cflag raw) sb-posix:cs8))
          (setf (sb-posix:termios-lflag raw)
                (logand (sb-posix:termios-lflag raw)
                        (lognot (logior sb-posix:echo sb-posix:icanon
                                        sb-posix:iexten sb-posix:isig))))
          (let ((cc (sb-posix:termios-cc raw)))
            (setf (aref cc sb-posix:vmin)  1
                  (aref cc sb-posix:vtime) 0))
          (sb-posix:tcsetattr fd sb-posix:tcsaflush raw)
          (setf *raw-mode-p* t))
      (error (e)
        (warn "Failed to enable raw mode: ~A" e)))))

(defun disable-raw-mode ()
  "Restore terminal to original settings."
  (when (and *raw-mode-p* *original-termios*)
    (handler-case
        (sb-posix:tcsetattr (sb-sys:fd-stream-fd sb-sys:*stdin*)
                            sb-posix:tcsaflush *original-termios*)
      (error (e)
        (warn "Failed to disable raw mode: ~A" e)))
    (setf *raw-mode-p* nil)))

;;; ── Terminal size ─────────────────────────────────────────────────

(defun terminal-size ()
  "Return (width height) of the terminal."
  (handler-case
      (sb-alien:with-alien ((buf (sb-alien:array (sb-alien:unsigned 8) 8)))
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "ioctl"
                                (function sb-alien:int sb-alien:int
                                          sb-alien:unsigned-long (* t)))
         (sb-sys:fd-stream-fd sb-sys:*stdin*)
         #x5413  ; TIOCGWINSZ
         (sb-alien:addr (sb-alien:deref buf 0)))
        (let ((rows (logior (sb-alien:deref buf 0) (ash (sb-alien:deref buf 1) 8)))
              (cols (logior (sb-alien:deref buf 2) (ash (sb-alien:deref buf 3) 8))))
          (if (and (> rows 0) (> cols 0))
              (list cols rows)
              '(80 24))))
    (error () '(80 24))))

;;; ── Alternate screen ──────────────────────────────────────────────

(defun enter-alternate-screen ()
  (format *terminal-io* "~C[?1049h~C[H" *escape* *escape*)
  (force-output *terminal-io*))

(defun leave-alternate-screen ()
  (format *terminal-io* "~C[?1049l" *escape*)
  (force-output *terminal-io*))

;;; ── TTY input ─────────────────────────────────────────────────────

(defun open-tty ()
  "Open /dev/tty for non-blocking byte reads."
  (unless (and *tty-stream* (open-stream-p *tty-stream*))
    (setf *tty-stream*
          (open "/dev/tty" :direction :input
                           :element-type '(unsigned-byte 8)
                           :if-does-not-exist :error))
    (let ((fd (sb-sys:fd-stream-fd *tty-stream*)))
      (sb-posix:fcntl fd sb-posix:f-setfl
                      (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                              sb-posix:o-nonblock)))))

(defun close-tty ()
  (when (and *tty-stream* (open-stream-p *tty-stream*))
    (close *tty-stream*)
    (setf *tty-stream* nil)))

(defun read-byte-from-fd (fd)
  "Read one byte from FD, return byte or NIL if EAGAIN."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buf))
    (let ((n (sb-unix:unix-read fd (sb-sys:vector-sap buf) 1)))
      (when (and n (= n 1))
        (aref buf 0)))))

(defun wait-for-byte (fd timeout)
  "Wait up to TIMEOUT seconds for a byte on FD. Return byte or NIL."
  (let ((start (get-internal-real-time))
        (limit (* timeout internal-time-units-per-second)))
    (loop
      (let ((b (read-byte-from-fd fd)))
        (when b (return b)))
      (when (>= (- (get-internal-real-time) start) limit)
        (return nil))
      (sleep 0.001))))

;;; ── Key event parsing ─────────────────────────────────────────────

(defun parse-csi-sequence (fd)
  "Parse a CSI escape sequence after ESC [ has been read."
  (let ((first-byte (or (read-byte-from-fd fd)
                        (progn (sleep 0.001) (read-byte-from-fd fd)))))
    (unless first-byte
      (return-from parse-csi-sequence (make-key-event :code :unknown)))
    (if (and (>= first-byte 64) (<= first-byte 126))
        ;; Immediate final byte
        (case first-byte
          (65 (make-key-event :code +key-up+))
          (66 (make-key-event :code +key-down+))
          (67 (make-key-event :code +key-right+))
          (68 (make-key-event :code +key-left+))
          (72 (make-key-event :code +key-home+))
          (70 (make-key-event :code +key-end+))
          (t  (make-key-event :code :unknown)))
        ;; Parameter bytes followed by final byte
        (let ((params (list (code-char first-byte)))
              (final nil))
          (loop
            (let ((b (read-byte-from-fd fd)))
              (unless b (return))
              (if (and (>= b 64) (<= b 126))
                  (progn (setf final b) (return))
                  (push (code-char b) params))))
          (let ((param-str (coerce (nreverse params) 'string)))
            (case final
              (65  (make-key-event :code +key-up+))
              (66  (make-key-event :code +key-down+))
              (67  (make-key-event :code +key-right+))
              (68  (make-key-event :code +key-left+))
              (72  (make-key-event :code +key-home+))
              (70  (make-key-event :code +key-end+))
              (126 (cond
                     ((string= param-str "3") (make-key-event :code +key-delete+))
                     ((string= param-str "5") (make-key-event :code +key-page-up+))
                     ((string= param-str "6") (make-key-event :code +key-page-down+))
                     (t (make-key-event :code :unknown))))
              (t   (make-key-event :code :unknown))))))))

(defun read-key-event ()
  "Read and parse a single key event from the TTY fd. Returns key-event or NIL."
  (let* ((fd (sb-sys:fd-stream-fd *tty-stream*))
         (byte (read-byte-from-fd fd)))
    (unless byte (return-from read-key-event nil))
    (cond
      ;; Escape or escape sequence
      ((= byte 27)
       (let ((next (wait-for-byte fd *escape-timeout*)))
         (cond
           ((null next) (make-key-event :code +key-escape+))
           ((= next 91) (parse-csi-sequence fd))  ; ESC [
           (t (make-key-event :char (code-char next) :alt-p t)))))
      ;; Control characters
      ((< byte 32)
       (cond
         ((= byte 13) (make-key-event :code +key-enter+))
         ((= byte 10) (make-key-event :char #\Newline))
         ((= byte 9)  (make-key-event :code +key-tab+))
         ((= byte 8)  (make-key-event :code +key-backspace+))
         (t (make-key-event :char (code-char (+ byte 96)) :ctrl-p t))))
      ;; DEL
      ((= byte 127)
       (make-key-event :code +key-backspace+))
      ;; Printable
      (t
       (make-key-event :char (code-char byte))))))

;;; ── Public input API ──────────────────────────────────────────────

(defun read-key ()
  "Block until a key is pressed, return key-event."
  (open-tty)
  (loop
    (let ((key (read-key-event)))
      (when key (return key)))
    (sleep 0.01)))

(defun read-key-with-timeout (timeout-ms)
  "Try to read a key within TIMEOUT-MS milliseconds. Returns key-event or NIL."
  (open-tty)
  (let ((start (get-internal-real-time))
        (limit (* timeout-ms (/ internal-time-units-per-second 1000))))
    (loop
      (let ((key (read-key-event)))
        (when key (return key)))
      (when (> (- (get-internal-real-time) start) limit)
        (return nil))
      (sleep 0.01))))

;;; ── with-raw-terminal macro ───────────────────────────────────────

(defmacro with-raw-terminal (&body body)
  "Execute BODY in raw mode with alternate screen. Cleans up on exit."
  `(progn
     (enter-alternate-screen)
     (enable-raw-mode)
     (cursor-hide)
     (unwind-protect
          (progn ,@body)
       (close-tty)
       (cursor-show)
       (disable-raw-mode)
       (leave-alternate-screen)
       (reset))))
