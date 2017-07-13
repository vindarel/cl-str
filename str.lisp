(in-package :cl-user)
(defpackage str
  (:use :cl)
  (:export
   :trim-left
   :trim-right
   :trim
   :join
   :split
   :repeat
   :replace-all
   :concat
   :empty?
   :emptyp
   :blank?
   :blankp
   :blank-str-p
   :blank-str?
   :words
   :unwords
   :lines
   :starts-with?
   :starts-with-p
   :ends-with?
   :ends-with-p
   :unlines
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

(defun split (separator s &key omit-nulls)
  "Split s into substring by separator (cl-ppcre takes a regex, we do not)."
  ;; cl-ppcre:split doesn't return a null string if the separator appears at the end of s.
  (let* ((val (concat s
                      (string separator)
                      ;; so we need an extra character, but not the user's.
                      (if (string-equal separator #\x) "y" "x")))
         (res (butlast (cl-ppcre:split (cl-ppcre:quote-meta-chars (string separator)) val))))
    (if omit-nulls
        (remove-if (lambda (it) (empty? it)) res)
        res)))

(defun words (s &key (limit 0))
  "Return list of words, which were delimited by white space. If the optional limit is 0 (the default), trailing empty strings are removed from the result list (see cl-ppcre)."
  (if (not s)
      nil
      (cl-ppcre:split "\\s+" (trim-left s) :limit limit)))

(defun unwords (strings)
  "Join the list of strings with a whitespace."
  (join " " strings))

(defun lines (s &key omit-nulls)
  "Split the string by newline characters and return a list of lines."
  (split #\NewLine s :omit-nulls omit-nulls))

(defun unlines (strings)
  "Join the list of strings with a newline character."
  (join (make-string 1 :initial-element #\Newline) strings))

(defun repeat (count s)
  "Make a string of S repeated COUNT times."
  (let ((result nil))
    (dotimes (i count)
      (setf result (cons s result)))
    (apply #'concat result)))

(defun replace-all (old new s)
  "Replace `old` by `new` in `s`. Arguments are not regexs."
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

(defun starts-with? (start s &key (ignore-case nil))
  "Return t if s starts with the substring 'start', nil otherwise."
  (when (>= (length s) (length start))
    (let ((fn (if ignore-case #'string-equal #'string=)))
      (funcall fn s start :start1 0 :end1 (length start)))))

;; An alias:
(setf (fdefinition 'starts-with-p) #'starts-with?)

(defun ends-with? (end s &key (ignore-case nil))
  "Return t if s ends with the substring 'end', nil otherwise."
  (when (>= (length s) (length end))
    (let ((fn (if ignore-case #'string-equal #'string=)))
      (funcall fn s end :start1 (- (length s) (length end))))))

(setf (fdefinition 'ends-with-p) #'ends-with?)
