(in-package :cl-user)
(defpackage cl-s
  (:use :cl
        :prove
        )
  (:export
   #:s-trim-left
   #:s-trim-right
   #:s-trim
   #:s-join
   #:s-split
   ))

(in-package :cl-s)

(defvar whitespaces '(#\Space #\Newline #\Backspace #\Tab
                      #\Linefeed #\Page #\Return #\Rubout))

(defun s-trim-left (s)
  "Remove whitespaces at the beginning of s."
  (string-left-trim whitespaces s))

(defun s-trim-right (s)
  "Remove whitespaces at the end of s."
  (string-right-trim whitespaces s))

(defun s-trim (s)
  "Remove whitespaces at the beginning and end of s."
  (string-trim whitespaces s))
;; To and from lists

(defun s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (format nil
          (concatenate 'string "~{~a~^" separator "~}")
          strings))

(defun s-split (separator s)
  "Split s into substring by separator (a regex)."
  (cl-ppcre:split separator s))
