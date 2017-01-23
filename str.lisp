(in-package :cl-user)
(defpackage str
  (:use :cl
        :prove
        )
  (:export
   :trim-left
   :trim-right
   :trim
   :join
   :split
   :replace-all
   :concat
   :blank?
   :blank-p
   :blank-str-p
   :blank-str?
   :--map
   ))

(in-package :str)

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
  (let ((separator (replace-all "~" "~~" separator)))
    (format nil
            (concatenate 'string "~{~a~^" separator "~}")
            strings)))

(defun split (separator s)
  "Split s into substring by separator (a regex)."
  (cl-ppcre:split separator s))

(defun replace-all (old new s)
  "Replace @c(old) by @c(new) in @c(s). Arguments are not regexs."
  (let* ((cl-ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old))) ;; treat metacharacters as normal.
    (cl-ppcre:regex-replace-all old s new)))

(defun empty? (s)
  "Is s nil or the empty string ?"
  (or (null s) (string-equal "" s)))

(defun emptyp (s)
  "Is s nil or the empty string ?"
  (empty? s))

(defun blank? (s)
  "Is s nil or only contains whitespaces ?"
  (or (null s) (string-equal "" (trim s))))

(defun blankp (s)
  "Is s nil or only contains whitespaces ?"
  (blank? s))

(defmacro --map (form list)
  `(mapcar (lambda (it) ,form) ,list))
