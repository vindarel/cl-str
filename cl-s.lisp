(in-package :cl-user)
(defpackage cl-s
  (:use :cl
        :prove
        )
  (:export
   #:s-trim-left
   #:s-trim-right
   #:s-trim
   #:s-replace
   #:s-join
   #:s-split
   #:s-replace
   #:s-concat
   ))

(in-package :cl-s)

(defvar whitespaces '(#\Space #\Newline #\Backspace #\Tab
                      #\Linefeed #\Page #\Return #\Rubout))

(defun s-trim-left (s)
  "Remove whitespaces at the beginning of s. "
  (string-left-trim whitespaces s))

(defun s-trim-right (s)
  "Remove whitespaces at the end of s."
  (string-right-trim whitespaces s))

(defun s-trim (s)
  "Remove whitespaces at the beginning and end of s.
@begin[lang=lisp](code)
(s-trim \"  foo \") ;; => \"foo\"
@end(code)"
  (string-trim whitespaces s))

(defun concat (&rest strings)
  "Join all the string arguments into one string."
  (apply #'concatenate 'string strings))

(defun s-join (separator strings)
  "Join all the strings in @c(strings) with @c(separator) in between.
@begin[lang=lisp](code)
(s-join \" \" '(\"hello\" \"cl\")) ;; => \"hello cl\"
@end(code)
"
  (format nil
          (concatenate 'string "~{~a~^" separator "~}")
          strings))

(defun s-split (separator s)
  "Split s into substring by separator (a regex)."
  (cl-ppcre:split separator s))

(defun s-replace (old new s)
  "Replace @c(old) by @c(new) in @c(s). Arguments are not regexs."
  (let* ((cl-ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old))) ;; treat metacharacters as normal.
    (cl-ppcre:regex-replace-all old s new)))
