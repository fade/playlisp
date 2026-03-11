;;; test-wild-m3u.lisp - Test parser against real-world M3U files

(load "/home/glenn/quicklisp/setup.lisp")
(ql:quickload '(:rutils :taglib :alexandria) :silent t)
(push #p"/home/glenn/SourceCode/playlisp/" asdf:*central-registry*)
(push #p"/home/glenn/SourceCode/parsector/" asdf:*central-registry*)
(ql:quickload :playlisp/parser :silent t)

(in-package :playlisp/parser)

(defun read-file-raw (path)
  "Read file using shell cat to avoid SBCL pathname pattern issues."
  (with-output-to-string (out)
    (let ((proc (sb-ext:run-program "/bin/cat" (list path)
                                    :output :stream
                                    :wait nil)))
      (unwind-protect
           (loop for line = (read-line (sb-ext:process-output proc) nil nil)
                 while line
                 do (write-line line out))
        (sb-ext:process-close proc)))))

(defun test-file (path)
  (format t "~%=== Testing: ~a ===~%" (subseq path (1+ (position #\/ path :from-end t))))
  (handler-case
      (let* ((content (read-file-raw path))
             (playlist (parse-m3u content)))
        (format t "SUCCESS: ~a tracks~%" (length (playlisp:playlist-elements playlist)))
        (dolist (track (subseq (playlisp:playlist-elements playlist) 
                               0 
                               (min 3 (length (playlisp:playlist-elements playlist)))))
          (format t "  ~a (~as) -> ~a~%" 
                  (playlisp:title track) 
                  (playlisp:runtime track) 
                  (playlisp:track-path track))))
    (error (e) (format t "ERROR: ~a~%" e))))

(format t "~%========================================~%")
(format t "Testing playlisp M3U parser on wild files~%")
(format t "========================================~%")

;; Extended M3U files (should work)
(test-file "/home/fade/Media/Music/Cut Copy - Haiku From Zero (2017) [FLAC] {2557864014}/Cut Copy - Haiku From Zero.m3u")
(test-file "/home/fade/Media/Music/Gary Numan/(2013) Splinter (Songs From A Broken Mind)/00-Gary_Numan-Splinter_(Songs_From_A_Broken_Mind)-2013-FLAC.m3u")
(test-file "/home/fade/Media/Music/The Orb - Adventures Beyond The Ultraworld (1991, 2006 3CD Deluxe Edition) (flac)/cd 1/The Orb - The Orb's Adventures Beyond The Ultraworld Deluxe Edition (Disc 1).m3u")
(test-file "/home/fade/Media/Music/Johann Johannsson - The Theory of Everything (2014) [FLAC]/Jóhann Jóhannsson - The Theory Of Everything.m3u")
(test-file "/home/fade/Media/Music/Lords Of Acid - Voodoo-U (1994) [FLAC]/Lords of Acid - Voodoo-U.m3u")
(test-file "/home/fade/Media/Music/Four Tet - Sixteen Oceans (2020) {Text Records - TEXT051} [CD FLAC]/Four Tet - Sixteen Oceans.m3u")
(test-file "/home/fade/Media/Music/Plaid - The Digging Remedy (2016) [FLAC]/Plaid - The Digging Remedy.m3u")

;; Simple M3U files (no #EXTM3U header - expected to fail)
(format t "~%--- Simple M3U files (no #EXTM3U header) ---~%")
(test-file "/home/fade/Media/Music/Ramones - Transmission Impossible 2015 FLAC/CD 1/play.m3u")
(test-file "/home/fade/Media/Music/Dead Voices on Air - Mirror Carrier (2018) [WEB-FLAC16-44]/-=Dead Voices on Air - Mirror Carrier (2018) [WEB-FLAC16-44].m3u")

(format t "~%========================================~%")
(format t "Testing complete~%")
(format t "========================================~%")
