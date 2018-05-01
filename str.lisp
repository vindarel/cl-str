(in-package :cl-user)
(defpackage str
  (:use :cl)
  (:export
   :contains?
   :containsp
   :trim-left
   :trim-right
   :trim
   :join
   :split
   :split-omit-nulls
   :substring
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
   :prefix
   :suffix
   :unlines
   :from-file
   :to-file
   :string-case
   :s-first
   :s-rest
   :s-nth
   :version
   :+version+
   ))

(in-package :str)

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defvar +version+ "0.8")

(defun version ()
  (print +version+))

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

(defun split-omit-nulls (separator s)
  "Call split with :omit-nulls to t.

   Can be clearer in certain situations.
  "
  (split separator s :omit-nulls t))

(defun substring (start end s)
  "Return the substring of `s' from `start' to `end'.

It uses `subseq' with differences:
- argument order, s at the end
- `start' and `end' can be lower than 0 or bigger than the length of s.
- for convenience `end' can be nil or t to denote the end of the string.
"
  (let* ((s-length (length s))
         (end (cond
                ((null end) s-length)
                ((eq end t) s-length)
                (t end))))
    (setf start (max 0 start))
    (if (> start s-length)
        ""
        (progn
          (setf end (min end s-length))
          (when (< end (- s-length))
            (setf end 0))
          (when (< end 0)
            (setf end (+ s-length end)))
          (if (< end start)
              ""
              (subseq s start end))))))

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

(defun contains? (substring s &key (ignore-case nil))
  "Return `t` if `s` contains `substring`, nil otherwise. Ignore the case with `:ignore-case t`.
A simple call to the built-in `search` (which returns the position of the substring)."
  (let ((a (if ignore-case
               (string-downcase substring)
               substring))
        (b (if ignore-case
               (string-downcase s)
               s)))
    ;; weird case: (search "" nil) => 0
    (if (and (blank? substring)
             (null s))
        nil
        (if (search a b)
            t))))

(setf (fdefinition 'containsp) #'contains?)

(defun prefix-1 (item1 item2)
  (subseq item1 0 (or (mismatch item1 item2) (length item1))))

(defun prefix (items)
  "Find the common prefix between strings.

   Uses the built-in `mismatch', that returns the position at which
   the strings fail to match.

   Example: `(str:prefix '(\"foobar\" \"foozz\"))` => \"foo\"

   - items: list of strings
   - Return: a string.

  "
  (when items
    (reduce #'prefix-1 items)))

(defun suffix-1 (item1 item2)
  (subseq item1 (or (mismatch item1 item2 :from-end t) 0)))

(defun suffix (items)
  "Find the common suffix between strings.

   Uses the built-in `mismatch', that returns the position at which
   the strings fail to match.

   Example: `(str:suffix '(\"foobar\" \"zzbar\"))` => \"bar\"

   - items: list of strings
   - Return: a string.

  "
  (when items
    (reduce #'suffix-1 items)))

(defun from-file (pathname &rest keys)
  "Read the file and return its content as a string.

   From v0.7 simply uses uiop:read-file-string. There is also read-file-lines.

Example: (str:from-file \"path/to/file.txt\" :external-format :utf-8)

- external-format: if nil, the system default. Can be bound to :utf-8.
"
  (uiop:read-file-string pathname keys))

(defun to-file (pathname s &key (if-exists :supersede) (if-does-not-exist :create))
  "Write string `s' to file `pathname'. If the file does not exist, create it (use `:if-does-not-exist'), if it already exists, replace its content (`:if-exists').

Returns the string written to file."
  (with-open-file (f pathname :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (write-sequence s f)))

(defmacro string-case (str &rest forms)
  "A case-like macro that works with strings (case works only with symbols).

  Example:

  (str:string-case input
    (\"foo\" (do something))
    (nil (print \"input is nil\")
    (otherwise (print \"none of the previous forms was caught\")))

  You might also like pattern matching. The example below with optima is very similar:

  (optima:match \"hey\"
    (\"hey\" (print \"it matched\"))
    (otherwise :nothing))

  Note that there is also http://quickdocs.org/string-case/.
  "
  ;; thanks koji-kojiro/cl-repl
  (let ((test (gensym)))
    `(let ((,test ,str))
       (cond
         ,@(loop :for (s  f) :in forms
              :if (stringp s) :collect `((string= ,test ,s) ,f)
              :else :if (string= s 'otherwise) :collect `(t ,f)
              :else :collect `((eql ,test ,s) ,f))))))

(defun s-first (s)
  "Return the first substring of `s'."
  (if (empty? s)
      ""
      (subseq s 0 1)))

(defun s-rest (s)
  "Return the rest substring of `s'."
  (if (empty? s)
      ""
      (subseq s 1)))

(defun s-nth (n s)
  "Return the nth substring of `s'."
  (cond ((or (empty? s) (minusp n)) "")
	((= n 0) (s-first s))
	(t (s-nth (1- n) (s-rest s)))))
