(in-package :cl-user)
(defpackage cl-s
  (:use :cl
        :prove
        )
  (:shadow :replace)
  (:export
   :trim-left
   :trim-right
   :trim
   :replace
   :join
   :split
   :replace
   :concat
   ))

(in-package :cl-s)

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defun trim-left (s)
  "Remove whitespaces at the beginning of s. "
  (string-left-trim *whitespaces* s))

(defun trim-right (s)
  "Remove whitespaces at the end of s."
  (string-right-trim *whitespaces* s))

(defun trim (s)
  "Remove whitespaces at the beginning and end of s.
@begin[lang=lisp](code)
(trim \"  foo \") ;; => \"foo\"
@end(code)"
  (string-trim *whitespaces* s))

(defun concat (&rest strings)
  "Join all the string arguments into one string."
  (apply #'concatenate 'string strings))

(defun join (separator strings)
  " "
  (let ((separator (replace "~" "~~" separator)))
    (format nil
            (concatenate 'string "~{~a~^" separator "~}")
            strings)))

(defun split (separator s)
  "Split s into substring by separator (a regex)."
  (cl-ppcre:split separator s))

(defun replace (old new s)
  "Replace @c(old) by @c(new) in @c(s). Arguments are not regexs."
  (let* ((cl-ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old))) ;; treat metacharacters as normal.
    (cl-ppcre:regex-replace-all old s new)))
