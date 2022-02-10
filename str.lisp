(in-package :cl-user)

(defpackage str
  (:use :cl)
  (:import-from :cl-change-case
                :no-case
                :camel-case
                :dot-case
                :header-case
                :param-case
                :pascal-case
                :path-case
                :sentence-case
                :snake-case
                :swap-case
                :title-case
                :constant-case)
  (:export
   ;; cl-change-case functions:
   ;; (we don't re-export all of them. Otherwise, use UIOP:define-package's :reexport)
   ;; (for example, we define downcasep instead of re-exporting string-lower-case-p)
   :no-case
   :camel-case
   :dot-case
   :header-case
   :param-case
   :pascal-case
   :path-case
   :sentence-case
   :snake-case
   :swap-case
   :title-case
   :constant-case

   ;; ours:
   :remove-punctuation
   :contains?
   :containsp
   :s-member
   :trim-left
   :trim-right
   :trim
   :collapse-whitespaces
   :join
   :insert
   :split
   :rsplit
   :split-omit-nulls
   :substring
   :shorten
   :prune ;; "deprecated" in favor of shorten
   :repeat
   :replace-first
   :replace-all
   :replace-using
   :concat
   :empty?
   :emptyp
   :non-empty-string-p
   :non-blank-string-p
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
   :common-prefix
   :prefix
   :suffix
   :prefix?
   :prefixp
   :suffix?
   :suffixp
   :add-prefix
   :add-suffix
   :pad
   :pad-left
   :pad-right
   :pad-center
   :unlines
   :from-file
   :to-file
   :string-case
   :s-first
   :s-last
   :s-rest
   :s-nth
   :s-assoc-value
   :count-substring

   :downcase
   :upcase
   :capitalize
   :downcasep
   :downcase?
   :upcasep
   :upcase?
   :has-alphanum-p
   :has-alpha-p
   :has-letters-p
   :alphanump
   :alphanum?
   :alphap
   :ascii-char-p
   :ascii-p
   :lettersp
   :letters?
   :lettersnump
   :alpha?
   :digitp
   :digit?

   :*ignore-case*
   :*omit-nulls*
   :*ellipsis*
   :*pad-char*
   :*pad-side*
   :version
   :+version+
   :?))

(in-package :str)


(defparameter *ignore-case* nil)
(defparameter *omit-nulls* nil)
(defparameter *pad-char* #\Space
  "Padding character to use with `pad'. It can be a string of one character.")
(defparameter *pad-side* :right
  "The side of the string to add padding characters to. Can be one of :right, :left and :center.")

(defvar *whitespaces* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defvar +version+ (asdf:component-version (asdf:find-system "str")))

(defun version ()
  (print +version+))

(defun trim-left (s)
  "Remove whitespaces at the beginning of s."
  (when s
    (string-left-trim *whitespaces* s)))

(defun trim-right (s)
  "Remove whitespaces at the end of s."
  (when s
    (string-right-trim *whitespaces* s)))

(defun trim (s)
  "Remove whitespaces at the beginning and end of s.
@begin[lang=lisp](code)
(trim \"  foo \") ;; => \"foo\"
@end(code)"
  (when s
    (string-trim *whitespaces* s)))

(defun collapse-whitespaces (s)
  "Ensure there is only one space character between words.
  Remove newlines."
  (ppcre:regex-replace-all "\\s+" s " "))

(declaim (ftype (function (&rest (or null string))
                          string)
                concat))
(defun concat (&rest strings)
  "Join all the string arguments into one string."
  (apply #'concatenate 'string strings))

(declaim (ftype (function ((or null character string)
                           (or null (cons string)))
                          string)
                join))
(defun join (separator strings)
  "Join all the strings of the list with a separator.

  `separator' can be a string or a character.

  Example:
  (str:join \",\" '(\"a\" \"b\" \"c\")
  => \"a,b,c\""
  (let ((sep (string separator)))
    (with-output-to-string (out)
      (loop for (s . rest) on strings
         do (write-string s out)
         unless (null rest)
         do (write-string sep out)))))

(defun insert (string/char index s)
  "Insert the given string (or character) at the `index' into `s' and return a new string.

  If `index' is out of bounds, ignore and return `s'."
  (when (characterp string/char)
    (setf string/char (string string/char)))
  (cond
    ((null index)
     s)
    ((< index 0)
     s)
    ((> index (length s))
     s)
    (t
     (concatenate 'string
                  (subseq s 0 index)
                  string/char
                  (subseq s index)))))

(defun split (separator s &key (omit-nulls *omit-nulls*) limit (start 0) end)
  "Split s into substring by separator (cl-ppcre takes a regex, we do not).

  `limit' limits the number of elements returned (i.e. the string is
  split at most `limit' - 1 times)."
  ;; cl-ppcre:split doesn't return a null string if the separator appears at the end of s.
  (let* ((limit (or limit (1+ (length s))))
         (res (ppcre:split `(:sequence ,(string separator)) s :limit limit :start start :end end)))
    (if omit-nulls
        (remove-if (lambda (it) (empty? it)) res)
        res)))

(defun rsplit (sep s &key (omit-nulls *omit-nulls*) limit)
  "Similar to `split`, except we split from the end. In particular,
the results will be be different when `limit` is provided."
  (nreverse
   (mapcar 'nreverse
           (split (reverse (string sep)) (reverse s)
                  :omit-nulls omit-nulls
                  :limit limit))))

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

(defparameter *ellipsis* "..."
  "Ellipsis to add to the end of a truncated string (see `shorten').")

(defun prune (len s &key (ellipsis *ellipsis*))
  "Old name for `shorten'."
  (shorten len s :ellipsis ellipsis))

(defun shorten (len s &key (ellipsis *ellipsis*))
  "If s is longer than `len', truncate it to this length and add the `*ellipsis*' at the end (\"...\" by default). Cut it down to `len' minus the length of the ellipsis."
  (when (and len
             (< len
                (length s)))
    (let ((end (max (- len (length ellipsis))
                    0)))
      (setf s (concat
               (subseq s 0 end)
               ellipsis))))
  s)

(defun words (s &key (limit 0))
  "Return list of words, which were delimited by white space. If the optional limit is 0 (the default), trailing empty strings are removed from the result list (see cl-ppcre)."
  (when s
      (ppcre:split "\\s+" (trim-left s) :limit limit)))

(defun unwords (strings)
  "Join the list of strings with a whitespace."
  (join " " strings))

(defun lines (s &key (omit-nulls *omit-nulls*))
  "Split the string by newline characters and return a list of lines. A terminal newline character does NOT result in an extra empty string."
  (when (and s (> (length s) 0))
    (let ((end (if (eql #\Newline (elt s (1- (length s))))
                   (1- (length s))
                   nil)))
     (split #\NewLine s :omit-nulls omit-nulls :end end))))

(defun unlines (strings)
  "Join the list of strings with a newline character."
  (join (make-string 1 :initial-element #\Newline) strings))

(defun repeat (count s)
  "Make a string of S repeated COUNT times."
  (let ((result nil))
    (dotimes (i count)
      (setf result (cons s result)))
    (apply #'concat result)))

(defun replace-first (old new s)
  "Replace the first occurence of `old` by `new` in `s`. Arguments are not regexs."
  (let* ((ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old))) ;; treat metacharacters as normal.
    ;; We need the (list new): see !52
    (ppcre:regex-replace old s (list new))))

(defun replace-all (old new s)
  "Replace all occurences of `old` by `new` in `s`. Arguments are not regexs."
  (let* ((ppcre:*allow-quoting* t)
         (old (concatenate 'string  "\\Q" old))) ;; treat metacharacters as normal.
    (ppcre:regex-replace-all old s (list new))))

;; About the (list new) above:
#+nil
(progn
  ;; This is wrong:
  (format t "~&This replacement is wrong: ~a~&" (ppcre:regex-replace-all "8" "foo8bar" "\\'"))
  ;; => foobarbar
  (format t "and this is OK: ~a~&" (ppcre:regex-replace-all "8" "foo8bar" (list "\\'")))
  ;; foo\'bar
  )

(defun replace-using (plist s)
  "Replace all associations given by pairs in a plist and return a new string.

  The plist is a list alternating a string to replace (case sensitive) and its replacement.

  Example:
  (replace-using (list \"{{phone}}\" \"987\")
                 \"call {{phone}}\")
  =>
  \"call 987\"

  It calls `replace-all' as many times as there are replacements to do."
  (check-type plist list)
  (dotimes (i (- (length plist)
                 1))
    (setf s (str:replace-all (nth i plist) (nth (incf i) plist) s)))
  s)

(defun empty? (s)
  "Is s nil or the empty string ?"
  (or (null s) (string-equal "" s)))

(defun emptyp (s)
  "Is s nil or the empty string ?"
  (empty? s))

(defun non-empty-string-p (s)
  "Return t if `s' is a string and is non-empty.

  Like `(not (empty? s))', with a `stringp' check. Useful in context."
  (and (stringp s)
       (not (emptyp s))))

(defun blank? (s)
  "Is s nil or only contains whitespaces ?"
  (or (null s) (string-equal "" (trim s))))

(defun blankp (s)
  "Is s nil or only contains whitespaces ?"
  (blank? s))

(defun non-blank-string-p (s)
  "Return t if `s' is a string and is non blank (it doesn't exclusively contain whitespace characters).

  Like `(not (blank? s))', with a `stringp' check. Useful in context."
  (and (stringp s)
       (not (blankp s))))

(defun starts-with? (start s &key (ignore-case *ignore-case*))
  "Return t if s starts with the substring 'start', nil otherwise."
  (when (>= (length s) (length start))
    (let ((fn (if ignore-case #'string-equal #'string=)))
      (funcall fn s start :start1 0 :end1 (length start)))))

;; An alias:
(setf (fdefinition 'starts-with-p) #'starts-with?)

(defun ends-with? (end s &key (ignore-case *ignore-case*))
  "Return t if s ends with the substring 'end', nil otherwise."
  (when (>= (length s) (length end))
    (let ((fn (if ignore-case #'string-equal #'string=)))
      (funcall fn s end :start1 (- (length s) (length end))))))

(setf (fdefinition 'ends-with-p) #'ends-with?)

(defun contains? (substring s &key (ignore-case *ignore-case*))
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

(defun common-prefix (items)
  (warn "common-prefix is deprecated, use prefix instead.")
  (prefix items))

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

(defun prefix? (items s)
  "Return s if s is common prefix between items."
  (when (string= s (prefix items)) s))

(setf (fdefinition 'prefixp) #'prefix?)

(defun suffix? (items s)
  "Return s if s is common suffix between items."
  (when (string= s (suffix items)) s))

(setf (fdefinition 'suffixp) #'suffix?)

(defun add-prefix (items s)
  "Prepend s to the front of each items."
  (mapcar #'(lambda (item) (concat s item)) items))

(defun add-suffix (items s)
  "Append s to the end of eahc items."
  (mapcar #'(lambda (item) (concat item s)) items))

(defun pad (len s &key (pad-side *pad-side*) (pad-char *pad-char*))
  "Fill `s' with characters until it is of the given length. By default, add spaces on the right.

Filling with spaces can be done with format:

    (format nil \"~v@a\" len s) ;; with or without the @ directive

`pad-side': to pad `:right' (the default), `:left' or `:center'.
`pad-char': padding character (or string of one character). Defaults to a space."
  (if (< len (length s))
      s
      (flet ((%pad-left (len s &key (pad-char *pad-char*))
               (concatenate 'string
                            (make-string (- len (length s)) :initial-element pad-char)
                            s))
             (%pad-right (len s &key (pad-char *pad-char*))
               (concatenate 'string
                            s
                            (make-string (- len (length s)) :initial-element pad-char)))
             (%pad-center (len s &key (pad-char *pad-char*))
               (multiple-value-bind (q r)
                   (floor (- len (length s)) 2)
                 (concatenate 'string
                              (make-string q :initial-element pad-char)
                              s
                              (make-string (+ q r) :initial-element pad-char)))))

        (unless (characterp pad-char)
          (if (>= (length pad-char) 2)
              (error "pad-char must be a character or a string of one character.")
              (setf pad-char (coerce pad-char 'character))))
        (case pad-side
          (:right
           (%pad-right len s :pad-char pad-char))
          (:left
           (%pad-left len s :pad-char pad-char))
          (:center
           (%pad-center len s :pad-char pad-char))
          (t
           (error "str:pad: unknown padding side with ~a" pad-side))))))

(defun pad-left (len s &key (pad-char *pad-char*))
  (pad len s :pad-side :left :pad-char pad-char))

(defun pad-right (len s &key (pad-char *pad-char*))
  (pad len s :pad-side :right :pad-char pad-char))

(defun pad-center (len s &key (pad-char *pad-char*))
  (pad len s :pad-side :center :pad-char pad-char))

(defun from-file (pathname &rest keys)
  "Read the file and return its content as a string.

It simply uses uiop:read-file-string. There is also uiop:read-file-lines.

Example: (str:from-file \"path/to/file.txt\" :external-format :utf-8)

- external-format: if nil, the system default. Can be bound to :utf-8.
"
  (apply #'uiop:read-file-string pathname keys))

(defun to-file (pathname s &key (if-exists :supersede) (if-does-not-exist :create))
  "Write string `s' to file `pathname'. If the file does not exist, create it (use `:if-does-not-exist'), if it already exists, replace its content (`:if-exists').

Returns the string written to file."
  (with-open-file (f pathname :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (write-sequence s f)))

(defmacro string-case (str &body forms)
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
  (if (null s)
      nil
      (if (empty? s)
          ""
          (subseq s 0 1))))

(defun s-last (s)
  "Return the last substring of `s'."
  (if (null s)
      nil
      (if (empty? s)
          ""
          (substring (1- (length s)) t s))))

(defun s-rest (s)
  "Return the rest substring of `s'."
  (if (null s)
      nil
      (if (empty? s)
          ""
          (subseq s 1))))

(defun s-nth (n s)
  "Return the nth substring of `s'.

   You could also use
   (string (elt \"test\" 1))
   ;; => \"e\""
  (cond ((null s) nil)
        ((or (empty? s) (minusp n)) "")
        ((= n 0) (s-first s))
        (t (s-nth (1- n) (s-rest s)))))

(defun s-assoc-value (alist key)
  "Return the value of a cons cell in `alist' with key `key', tested
with `string='.
  The second return value is the cons cell."
  (let ((cons (assoc key alist :test #'string-equal)))
    (values (cdr cons) cons)))

(defun s-member (list s &key (test #'string=)
                          (ignore-case *ignore-case*))
  "Return T if `s' is a member of `list'. Do not ignore case by default.

  NOTE: S-MEMBER's arguments' order is the reverse of CL:MEMBER.

  If `:ignore-case' or `*ignore-case*' are not nil, ignore case (using `string-equal' instead of `string=').

  Unlike CL:MEMBER, S-MEMBER returns T or NIL, instead of the tail of LIST whose first element satisfies the test."
   ;; Maybe: have `str:member' for the same argument order.
  (when (member s list
                :test (if ignore-case #'string-equal test))
    t))

(defun count-substring (substring s &key (start 0) (end nil))
  "Return the non-overlapping occurrences of `substring' in `s'.
  You could also count only the ocurrencies between `start' and `end'.

  Examples:
  (count-substring \"abc\" \"abcxabcxabc\")
  ;; => 3

  (count-substring \"abc\" \"abcxabcxabc\" :start 3 :end 7)
  ;; => 1"
  (unless (or (null s)
              (null substring)
              (empty? substring))
    (loop :with substring-length := (length substring)
       :for position := (search substring s :start2 start :end2 end)
       :then (search substring s :start2 (+ position substring-length) :end2 end)
       :while (not (null position))
       :summing 1)))


;;; Case

;; Small wrappers around built-ins, but they fix surprises.

(defun downcase (s)
  "Return the lowercase version of `s'.
  Calls the built-in `string-downcase', but returns nil if `s' is
  nil (instead of the string \"nil\")."
  (unless (null s)
    (string-downcase s)))

(defun upcase (s)
  "Return the uppercase version of `s'.
  Call the built-in `string-upcase', but return nil if `s' is
  nil (instead of the string \"NIL\")."
  (unless (null s)
    (string-upcase s)))

(defun capitalize (s)
  "Return the capitalized version of `s'.
  Calls the built-in `string-capitalize', but returns nil if `s' is
  nil (instead of the string \"Nil\")."
  (unless (null s)
    (string-capitalize s)))

;;; Case predicates.

(defun alphanump (s)
  "Return t if `s' contains at least one character and all characters are alphanumeric.
  See also `lettersnump' which also works on unicode letters."
  (ppcre:scan "^[a-zA-Z0-9]+$" s))

(defun alphanum? (s)
  (alphanump s))

(defun alphap (s)
  "Return t if `s' contains at least one character and all characters are alpha (in [a-zA-Z]).
  See also `lettersp', which checks for unicode letters."
  (ppcre:scan-to-strings "^[a-zA-Z]+$" s)
  ;; TODO: this regexp accepts é and ß: in lettersp like cuerdas ?
  ;; and like in python, so definitely yes.
  ;; (ppcre:scan-to-strings "^\\p{L}+$" s)
  )

(defun alpha? (s)
  (alphap s))

(defun lettersp (s)
  "Return t if `s' contains only letters (including unicode letters).

   (alphap \"éß\") ;; => nil
   (lettersp \"éß\") ;; => t"
  (when (ppcre:scan "^\\p{L}+$" s)
    t))

(defun letters? (s)
  (lettersp s))

(defun lettersnump (s)
  "Return t if `s' contains only letters (including unicode letters) and digits."
  (when (ppcre:scan "^[\\p{L}a-zA-Z0-9]+$" s)
    t))

(defun digitp (s)
  "Return t if `s' contains at least one character and all characters are numerical."
  (unless (emptyp s)
    ;; regex ? Check sign and exponents.
    (every (lambda (char)
             (digit-char-p char))
           s)))

(defun digit? (s)
  (digitp s))


(defun numericp (s)
  "alias for `digitp'."
  (digitp s))

(defun numeric? (s)
  (numericp s))

(defun has-alphanum-p (s)
  "Return t if `s' has at least one alphanumeric character."
  (unless (emptyp s)
    (some (lambda (char)
            (alphanumericp char))
          s)))

(defun has-alpha-p (s)
  "Return t if `s' has at least one alpha character ([a-zA-Z])."
  (when (ppcre:scan "[a-zA-Z]" s)
    t))

(defun has-letters-p (s)
  "Return t if `s' contains at least one letter (considering unicode, not only alpha characters)."
  (when (ppcre:scan "\\p{L}" s)
    t))

(declaim (inline ascii-char-p))
(defun ascii-char-p (char)
  "Return t if `char' is an ASCII char (its char code is below 128)."
  ;; Inspired by Serapeum
  (when (and (< (char-code char) 128)
             char)
    t))

(declaim (ftype (function ((or null character string))
                          boolean)
                ascii-p))
(declaim (inline ascii-p))
(defun ascii-p (char/s)
  "If `char/s' is a character, return t if it is an ASCII character (its char code is below 128).
   If `char/s' is a string, return t if every character is ASCII."
  ;; Inspired by Serapeum.
  (declare (type (or null character string) char/s))
  (when char/s
    (typecase char/s
      (character
       (ascii-char-p char/s))
      (string
       ;; we could return the string itself as it is usually done on CL functions.
       (every #'ascii-char-p char/s)))))

(defun downcasep (s)
  "Return t if all alphabetical characters of `s' are lowercase, and `s' contains at least one letter."
  (if (has-letters-p s)
      (every (lambda (char)
               (if (alpha-char-p char)
                   (lower-case-p char)
                   t))
             s)))

(defun downcase? (s)
  "alias for `downcasep'."
  (downcasep s))

(defun upcasep (s)
  "Return t if all alphabetical characters of `s' are uppercase."
  (if (has-letters-p s)
    (every (lambda (char)
             (if (alpha-char-p char)
                 (upper-case-p char)
                 t))
           s)))

(defun upcase? (s)
  "alias for `upcasep'."
  (upcasep s))

(defun remove-punctuation (s &key (replacement " "))
  "Remove the punctuation characters from `s', replace them with `replacement' (defaults to a space) and strip continuous whitespace."
  (flet ((replace-non-word (string)
           (ppcre:regex-replace-all
            "[^\\p{L}\\p{N}]+"
            string
            (lambda (target start end match-start match-end reg-starts reg-ends)
              (declare (ignore target start reg-starts reg-ends))
              ;; completely remove trailing and leading non-word chars
              (if (or (zerop match-start)
                      (= match-start (- end (- match-end match-start))))
                  ""
                  ;; use replacement kwarg for non-space chars inbetween
                  replacement)))))
    (if (null s)
        ""
        (replace-non-word s))))
