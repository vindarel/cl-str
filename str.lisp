(in-package :cl-user)

(defpackage str
  (:use :cl)
  (:export
  #:remove-punctuation
  #:containsp
  #:s-member
  #:trim-left
  #:trim-right
  #:trim
  #:collapse-whitespaces
  #:join
  #:insert
  #:split
  #:rsplit
  #:split-omit-nulls
  #:substring
  #:shorten
  #:repeat
  #:replace-first
  #:replace-all
  #:replace-using
  #:concat
  #:emptyp
  #:non-empty-string-p
  #:non-blank-string-p
  #:blankp
  #:blank-str-p
  #:words
  #:unwords
  #:lines
  #:starts-with-p
  #:ends-with-p
  #:prefix
  #:suffix
  #:prefixp
  #:suffixp
  #:add-prefix
  #:add-suffix

  #:ensure
  #:ensure-prefix
  #:ensure-suffix
  #:ensure-wrapped-in
  #:wrapped-in-p

  #:pad
  #:pad-left
  #:pad-right
  #:pad-center
  #:fit
  #:unlines
  #:from-file
  #:to-file
  #:string-case
  #:match
  #:s-first
  #:s-last
  #:s-rest
  #:s-nth
  #:s-assoc-value
  #:count-substring

  ;; case-related functions:
  #:downcase
  #:upcase
  #:capitalize
  #:no-case
  #:camel-case
  #:dot-case
  #:header-case
  #:param-case
  #:pascal-case
  #:path-case
  #:sentence-case
  #:snake-case
  #:swap-case
  #:title-case
  #:constant-case

  ;; predicate functions
  #:downcasep
  #:upcasep
  #:has-alphanum-p
  #:has-alpha-p
  #:has-letters-p
  #:alphanump
  #:alphap
  #:ascii-char-p
  #:ascii-p
  #:lettersp
  #:lettersnump
  #:digitp
  #:numericp ;; An alias for digitp

  ;; "deprecated" alias for predicates
  #:empty?
  #:blank?
  #:starts-with?
  #:ends-with?
  #:contains?
  #:prefix?
  #:suffix?
  #:alphanum?
  #:alpha?
  #:letters?
  #:digit?
  #:numeric?
  #:downcase?
  #:upcase?
  #:prune ;; "deprecated" alias for shorten
  #:common-prefix ;; "deprecated" alias for prefix

  #:*ignore-case*
  #:*omit-nulls*
  #:*ellipsis*
  #:*pad-char*
  #:*pad-side*
  #:*whitespaces*
  #:version
  #:+version+
  #:?
  #:unparagraphs
  #:paragraphs))

(in-package :str)


(defparameter *ignore-case* nil)
(defparameter *omit-nulls* nil)
(defparameter *pad-char* #\Space
  "Padding character to use with `pad'. It can be a string of one character.")
(defparameter *pad-side* :right
  "The side of the string to add padding characters to. Can be one of :right, :left and :center.")

(defvar *whitespaces* (list #\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page
                            #\Return #\Space #\Rubout
                            #+sbcl #\Next-Line #-sbcl (code-char 133)
                            #+(or abcl gcl lispworks ccl) (code-char 12288) #-(or abcl gcl lispworks ccl) #\Ideographic_space
                            #+lispworks #\no-break-space #-lispworks #\No-break_space)
  "On some implementations, linefeed and newline represent the same character (code).")

(defvar *newline* #\Newline "Newline character")

(defvar +version+ (asdf:component-version (asdf:find-system "str")))

(defun version ()
  (print +version+))

(defun trim-left (s &key (char-bag *whitespaces*))
  "Removes all characters in `char-bag` (default: whitespaces) at the beginning of `s`.
   If supplied, char-bag has to be a sequence (e.g. string or list of characters).

   Examples: (trim-left \"  foo \") => \"foo \"
             (trim-right \"+-*foo-bar*-+\" :char-bag \"+-*\") => \"foo-bar*-+\"
             (trim-left \"afood\" :char-bag (list #\a #\d) ;; => \"food\""
  (when s
    (string-left-trim char-bag s)))

(defun trim-right (s &key (char-bag *whitespaces*))
  "Removes all characters in `char-bag` (default: whitespaces) at the end of `s`.
   If supplied, char-bag has to be a sequence (e.g. string or list of characters).

   Examples: (trim-right \"  foo \") => \"  foo\"
             (trim-right \"+-*foo-bar*-+\" :char-bag \"+-*\") => \"+-*foo-bar\"
             (trim-right \"afood\" :char-bag (list #\a #\d) ;; => \"afoo\""
  (when s
    (string-right-trim char-bag s)))

(defun trim (s &key (char-bag *whitespaces*))
  "Removes all characters in `char-bag` (default: whitespaces) at the beginning and end of `s`.
   If supplied, char-bag has to be a sequence (e.g. string or list of characters).

   Examples: (trim \"  foo \") => \"foo\"
             (trim \"+-*foo-bar*-+\" :char-bag \"+-*\") => \"foo-bar\"
             (trim \"afood\" :char-bag (str:concat \"a\" \"d\")) => \"foo\""
  (when s
    (string-trim char-bag s)))

(defun collapse-whitespaces (s)
  "Ensure there is only one space character between words.
  Remove newlines."
  (ppcre:regex-replace-all "\\s+" s " "))

(declaim
 (inline concat)
 (ftype (function (&rest (or null string))
                  string)
        concat))
(defun concat (&rest strings)
  "Join all the string arguments into one string."
  (apply #'concatenate 'string strings))

(declaim
 (inline join)
 (ftype (function ((or null string symbol character)
                   (or null (cons string list)))
                  string)
        join))
(defun join (separator strings)
  "Join all the strings of the list with a separator.

  `separator' can be a string or a character.

  Example:
  (str:join \",\" '(\"a\" \"b\" \"c\")
  => \"a,b,c\""
  (let ((sep (if separator
                 (string separator)
                 "")))
    (with-output-to-string (out)
      (loop
        for (s . rest) on strings
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


;; Inline before first use.
(declaim (inline emptyp))
(defun emptyp (s)
  "Is s nil or the empty string ?"
  (or (null s) (string-equal "" s)))


(defun split (separator s &key (omit-nulls *omit-nulls*) limit (start 0) end regex)
  "Split s into substring by separator (cl-ppcre takes a regex, we do not).

   `limit' limits the number of elements returned (i.e. the string is
   split at most `limit' - 1 times).
   If `regex' is not nil, `separator' is treated as a regular expression.

   Examples:
   (str:split \",\" \"foo,bar\") ;; => (\"foo\" \"bar\")
   (str:split \"[,|;]\" \"foo,bar;baz\" :regex t) ;; => (\"foo\" \"bar\" \"baz\")
"
  (declare (type (or integer null) start end limit))
  ;; cl-ppcre:split doesn't return a null string if the separator appears at the end of s.
  (let* ((limit (or limit (1+ (length s))))
         (res (if regex
                  (ppcre:split separator s :limit limit :start start :end end)
                  (ppcre:split `(:sequence ,(string separator)) s :limit limit :start start :end end))))
    (if omit-nulls
        (delete-if (lambda (it) (declare (string it)) (emptyp it)) res)
        res)))

(defun rsplit (sep s &key (omit-nulls *omit-nulls*) limit regex)
  "Similar to `split', except we split from the end. In particular,
the results will be be different when `limit` is provided.

   If `regex' is not `nil`, `separator' is treated as a regular expression."
  (nreverse
   (mapcar 'nreverse
           (split (if regex
                      (string sep)
                      (reverse (string sep)))
                  (reverse s)
                  :omit-nulls omit-nulls
                  :limit limit
                  :regex regex))))

(defun split-omit-nulls (separator s &key regex)
  "Call `split' with :omit-nulls to t.

   Can be clearer in certain situations.

   If `regex' is not nil, `separator' is treated as a regular expression."
  (split separator s :omit-nulls t :regex regex))

(declaim (ftype (function (t t (or string null)) (or string null)) substring))
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

(declaim
 (ftype (function ((or number null)
                   (or string null)
                   &key (:ellipsis (or string null)))
                  (or string null))
        shorten))
(defun shorten (len s &key (ellipsis *ellipsis*))
  "If s is longer than `len', truncate it to this length and add the `*ellipsis*' at the end (\"...\" by default). Cut it down to `len' minus the length of the ellipsis."
  (if (and len
           (< len
              (length s)))
      (let ((end (max (- len (length ellipsis)) 0)))
        (concat (subseq s 0 end) ellipsis))
      ;; Return `s' if it doesn't need to be changed.
      s))

(defun words (s &key (limit 0))
  "Return list of words, which were delimited by white space. If the optional limit is 0 (the default), trailing empty strings are removed from the result list (see cl-ppcre)."
  (when s
      (ppcre:split "\\s+" (trim-left s) :limit limit)))

(defun unwords (strings)
  "Join the list of strings with a whitespace."
  (join " " strings))

(defun lines (s &key (omit-nulls *omit-nulls*))
  "Split the string by newline characters and return a list of lines. A terminal newline character does NOT result in an extra empty string."
  (let ((s-length (length s)))
    (when (and s (> s-length 0))
      (let ((end (if (eql #\Newline (elt s (1- s-length)))
                     (1- s-length)
                     nil)))
        (split #\NewLine s :omit-nulls omit-nulls :end end)))))

(defun unlines (strings)
  "Join the list of strings with a newline character."
  (join (make-string 1 :initial-element #\Newline) strings))

(defun paragraphs (string)
  "Split the string by paragraphs. Paragraphs are separated by two new lines.
  Return a list of strings. Each paragraph has whitespace strimmed around it."
  (mapcar #'str:trim (ppcre:split "\\n\\n" string)))

(defun unparagraphs (strings)
  "Join the list of strings by two newlines (have them separated by a blank line)."
  (let ((separator (concatenate 'string (list *newline* *newline*))))
    (join separator strings)))

(declaim (ftype (function (integer (or string null))
                          string)
                repeat))

(defun repeat (count string)
  "Make a string of S repeated COUNT times."
  (let ((len (length string)))
    (cond
      ((= len 0)
       "")
      ((= len 1)
       (let ((char (char string 0)))
         (make-string count :initial-element char :element-type (type-of char))))
      (t
       (let* ((index 0)
              (element-type (if (typep string 'base-string) ; maybe always make it character
                                'base-char
                                'character))
              (full-len (* count len))
              (full-string (make-string full-len :element-type element-type)))
         (dotimes (x count)
           (replace full-string string :start1 index)
           (incf index len))
         full-string)))))


(defun replace-first (old new s &key regex)
  "Replace the first occurence of `old` by `new` in `s`.

  By default, metacharacters are treated as normal characters.
  If `regex' is not nil, `old' is treated as a regular expression.

  Examples:
  (replace-first \"aa\" \"oo\" \"faaaa\") => \"fooaa\"
  (replace-first \"fo+\" \"frob\" \"foofoo bar\" :regex t) => \"frobfoo bar\""
    (if regex
        (ppcre:regex-replace old s new)
        (let ((ppcre:*allow-quoting* t))
          ;; We need the (list new): see !52
          (ppcre:regex-replace (concatenate 'string "\\Q" old) s (list new)))))

(defun replace-all (old new s &key regex)
  "Replace all occurences of `old' by `new' in `s'.

  By default, metacharacters are treated as normal characters.
  If `regex' is not nil, `old' is treated as a regular expression.

  Examples:
  (replace-all \"+\" \"'\\'\" \"foo+bar\") ;; => \"foo'\\'bar\"
  (replace-all \"fo+\" \"frob\" \"foofoo bar\" :regex t) ;; => \"frobfrob bar\""
    (if regex
        (ppcre:regex-replace-all old s new)
        (let ((ppcre:*allow-quoting* t))
          (ppcre:regex-replace-all (concatenate 'string "\\Q" old) s (list new)))))

;; About the (list new) above:
#+nil
(progn
  ;; This is wrong:
  (format t "~&This replacement is wrong: ~a~&" (ppcre:regex-replace-all "8" "foo8bar" "\\'"))
  ;; => foobarbar
  (format t "and this is OK: ~a~&" (ppcre:regex-replace-all "8" "foo8bar" (list "\\'")))
  ;; foo\'bar
  )

(defun replace-using (replacement-list s &key regex)
  "Replace all associations given by pairs in a list and return a new string.

  The `replacement-list' alternates a string to replace (case sensitive) and its replacement.
  By default, metacharacters in the string to replace are treated as normal characters.
  If `regex' is not nil, strings to replace are treated as regular expressions.

  Example:
  (replace-using (list \"{{phone}}\" \"987\")
                 \"call {{phone}}\")
  => \"call 987\"

  (replace-using (list \"fo+\" \"frob\"
                       \"ba+\" \"Bob\")
                 \"foo bar\"
                 :regex t)
  => \"frob Bobr\"

  It calls `replace-all' as many times as there are replacements to do."
  (loop for remaining-list on replacement-list by #'cddr do
    (let ((key (first remaining-list))
          (value (second remaining-list)))
      (declare (string key value))
      (setf s (replace-all key value s :regex regex))))
  s)

(declaim (inline non-empty-string-p))
(defun non-empty-string-p (s)
  "Return t if `s' is a string and is non-empty.

  Like `(not (emptyp s))', with a `stringp' check. Useful in context."
  (and (stringp s)
       (not (emptyp s))))

(declaim (inline blankp))
(defun blankp (s)
  "Is s nil or only contains whitespaces ?"
  (or (null s) (string-equal "" (trim s))))

(declaim (inline non-blank-string-p))
(defun non-blank-string-p (s)
  "Return t if `s' is a string and is non blank (it doesn't exclusively contain whitespace characters).

  Like `(not (blankp s))', with a `stringp' check. Useful in context."
  (and (stringp s)
       (not (blankp s))))

(defun starts-with-p (start s &key (ignore-case *ignore-case*))
  "Return t if S starts with the substring `START', nil otherwise.

  START can be a string or a character."
  (let ((start-length (length (string start))))
    (when (>= (length s) start-length)
      (let ((fn (if ignore-case #'string-equal #'string=)))
        (funcall fn s start :start1 0 :end1 start-length)))))

(defun ends-with-p (end s &key (ignore-case *ignore-case*))
  "Return t if s ends with the substring `END', nil otherwise.

  END can be a character or a string."
  (let ((s-length (length s))
        (end-length (length (string end))))
    (when (>= s-length end-length)
      (let ((fn (if ignore-case #'string-equal #'string=)))
        (funcall fn s end :start1 (- s-length end-length))))))

(declaim (ftype (function ((or string null) (or string null) &key (:ignore-case boolean)) boolean) containsp))
(defun containsp (substring s &key (ignore-case *ignore-case*))
  "Return `t` if `s` contains `substring`, nil otherwise. Ignore the case with `:ignore-case t`.
A simple call to the built-in `search` (which returns the position of the substring)."
  (when (and substring s)
    (let ((a (if ignore-case
                 (string-downcase substring)
                 substring))
          (b (if ignore-case
                 (string-downcase s)
                 s)))
      ;; weird case: (search "" nil) => 0
      (cond ((and (blankp substring)
                  (null s))
             nil)
            ((search a b)
             t)))))

(defun prefix-1 (item1 item2)
  (subseq item1 0 (or (mismatch item1 item2) (length item1))))

(declaim (inline prefix))
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

(declaim (inline suffix))
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

(declaim (inline prefixp))
(defun prefixp (items prefix)
  "Return PREFIX if all ITEMS start with it."
  (when (every (lambda (s)
                 (str:starts-with-p prefix s))
               items)
    prefix))

(declaim (inline suffixp))
(defun suffixp (items suffix)
  "Return `suffix' if all items end with it.
  Otherwise, return nil"
  (when (every (lambda (s)
               (str:ends-with-p suffix s))
             items)
    suffix))

(declaim (inline add-prefix))
(defun add-prefix (items s)
  "Prepend s to the front of each item."
  (mapcar (lambda (item) (concat s item)) items))

(declaim (inline add-suffix))
(defun add-suffix (items s)
  "Append s to the end of each item."
  (mapcar (lambda (item) (concat item s)) items))

(declaim (inline ensure-prefix))
(defun ensure-prefix (start s)
  "Ensure that `s' starts with `start'.
  Return a new string with its prefix added, if necessary.

Example:

  (str:ensure-prefix \"/\" \"abc/\") ;; => \"/abc/\"
  (str:ensure-prefix \"/\" \"/abc/\") ;; => \"/abc/\" (does nothing)

See also `str:ensure-suffix' and `str:ensure-wrapped-in'."
  (cond
    ((null start)
     s)
    ((null s)
     s)
    (t
     (let ((start-s (string start)))
       (if (not (str:starts-with-p start-s s))
           (str:concat start-s s)
           s)))))

(declaim (inline ensure-suffix))
(defun ensure-suffix (end s)
  "Ensure that `s' ends with `end'.
Return a new string with its suffix added, if necessary.

Example:

  (str:ensure-suffix \"/\" \"/abc\") ;; => \"/abc/\"
  (str:ensure-suffix \"/\" \"/abc/\") ;; => \"/abc/\" (does nothing)

See also `str:ensure-prefix' and `str:ensure-wrapped-in'."
  (cond
    ((null end)
     s)
    ((null s)
     s)
    (t
     (let ((end-s (string end)))
       (if (not (str:ends-with-p end-s s))
           (str:concat s end-s)
           s)))))

(declaim (inline ensure-wrapped-in))
(defun ensure-wrapped-in (start/end s)
  "Ensure that S starts and ends with `START/END'.
Return a new string.

  Example:

    (str:ensure-wrapped-in \"/\" \"abc\") ;; => \"/abc/\"
    (str:ensure-wrapped-in \"/\" \"/abc/\") ;; => \"/abc/\" (does nothing)

  See also: `str:enclosed-by-p'."
  (str:ensure-prefix start/end (str:ensure-suffix start/end s)))

(declaim (inline ensure))
(defun ensure (s &key wrapped-in prefix suffix)
  "The ensure functions return a string that has the specified prefix or suffix, appended if necessary.

This function looks for the following parameters, in order:

- :wrapped-in : if non nil and non empty, call STR:ENSURE-WRAPPED-IN.
- :prefix and :suffix : if both are supplied and non-nil, call STR:ENSURE-SUFFIX followed by STR:ENSURE-PREFIX.
- :prefix : call STR:ENSURE-PREFIX
- :suffix : call STR:ENSURE-SUFFIX.

warn: if both :wrapped-in and :prefix (and/or :suffix) are supplied together, :wrapped-in takes precedence and :prefix (and/or :suffix) is ignored.

Example:

    (str:ensure \"abc\" :wrapped-in \"/\")  ;; => \"/abc/\"
    (str:ensure \"/abc\" :prefix \"/\")  ;; => \"/abc\"  (no change, still one \"/\")
    (str:ensure \"/abc\" :suffix \"/\")  ;; => \"/abc/\"  (added a \"/\" suffix)

    These fonctions accept strings and characters:

    (str:ensure \"/abc\" :prefix #\\/)
"
  (cond
    ((and wrapped-in
          (str:non-empty-string-p wrapped-in))
     (ensure-wrapped-in wrapped-in s))
    ((and prefix suffix)
     (ensure-prefix prefix (ensure-suffix suffix s)))
    (prefix
     (ensure-prefix prefix s))
    (suffix
     (ensure-suffix suffix s))
    (t
     s)))

(declaim (inline wrapped-in-p))
(defun wrapped-in-p (start/end s)
  "Does S start and end with `START/END'?
If true, return S. Otherwise, return nil.

Example:

    (str:wrapped-in-p \"/\" \"/foo/\"  ;; => \"/foo/\"
    (str:wrapped-in-p \"/\" \"/foo\"  ;; => nil

See also: UIOP:STRING-ENCLOSED-P (prefix s suffix).
"
  (cond
    ((null start/end)
     s)
    ((null s)
     s)
    (t
     ;; (starts-with-p nil "foo") returns NIL.
     (when (and (str:starts-with-p start/end s)
                (str:ends-with-p start/end s))
       s))))

(declaim (inline pad))
(defun pad (len s &key (pad-side *pad-side*) (pad-char *pad-char*))
  "Fill `s' with characters until it is of the given length. By default, add spaces on the right.

Filling with spaces can be done with format:

    (format nil \"~v@a\" len s) ;; with or without the @ directive

`pad-side': to pad `:right' (the default), `:left' or `:center'.
`pad-char': padding character (or string of one character). Defaults to a space."
  (let ((s-length (length s)))
    (if (< len s-length)
        s
        (flet ((%pad-left ()
                 (concatenate 'string
                              (make-string (- len s-length) :initial-element pad-char)
                              s))
               (%pad-right ()
                 (concatenate 'string
                              s
                              (make-string (- len s-length) :initial-element pad-char)))
               (%pad-center ()
                 (multiple-value-bind (q r)
                     (floor (- len s-length) 2)
                   (concatenate 'string
                                (make-string q :initial-element pad-char)
                                s
                                (make-string (+ q r) :initial-element pad-char)))))
          (unless (characterp pad-char)
            (if (>= (length pad-char) 2)
                (error "pad-char must be a character or a string of one character.")
                (setf pad-char (coerce pad-char 'character))))
          (case pad-side
            (:right (%pad-right))
            (:left (%pad-left))
            (:center (%pad-center))
            (t (error "str:pad: unknown padding side with ~a" pad-side)))))))

(declaim (inline pad-left))
(defun pad-left (len s &key (pad-char *pad-char*))
  (pad len s :pad-side :left :pad-char pad-char))

(declaim (inline pad-right))
(defun pad-right (len s &key (pad-char *pad-char*))
  (pad len s :pad-side :right :pad-char pad-char))

(declaim (inline pad-center))
(defun pad-center (len s &key (pad-char *pad-char*))
  (pad len s :pad-side :center :pad-char pad-char))

(declaim (inline fit))
(defun fit (len s &key (pad-char *pad-char*) (pad-side :right) (ellipsis *ellipsis*))
  "Fit this string to the given length:
  - if it's too long, shorten it (showing the `ellipsis'),
  - if it's too short, add paddding (to the side `pad-side', adding the character `pad-char')."
  (assert  (and len
                (numberp len))
           nil
           "str:fit error: the given LEN must be a number.")
  (let ((s-length (length s)))
    (cond
      ((= s-length len)
       s)
      ((> s-length len)
       (shorten len s :ellipsis ellipsis))
      ((< s-length len)
       (pad len s :pad-side pad-side :pad-char pad-char)))))

(declaim (inline from-file))
(defun from-file (pathname &rest keys)
  "Read the file and return its content as a string.

It simply uses uiop:read-file-string. There is also uiop:read-file-lines.

Example: (str:from-file \"path/to/file.txt\" :external-format :utf-8)

- external-format: if nil, the system default. Can be bound to :utf-8.
"
  (apply #'uiop:read-file-string pathname keys))

(declaim (inline to-file))
(defun to-file (pathname s &key (if-exists :supersede) (if-does-not-exist :create))
  "Write string `s' to file `pathname'. If the file does not exist, create it (use `:if-does-not-exist'), if it already exists, replace its content (`:if-exists').

Returns the string written to file."
  (with-open-file (f pathname :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (write-sequence s f)))

(defmacro string-case (str &body forms)
  "A case-like macro that works with strings (case works only with symbols).
  You can either supply single-item clauses, multiple-items clauses, or otherwise.

  Example:

  (str:string-case input
    (\"foo\" (do something))
    ((\"hello\" \"test\") 5)
    (nil (print \"input is nil\")
    (otherwise (print \"none of the previous forms was caught\")))

  You might also like pattern matching with `str:match'.

  Note that there is also http://quickdocs.org/string-case/.
  "
  ;; thanks koji-kojiro/cl-repl
  (let ((test (gensym)))
    `(let ((,test ,str))
       (cond
         ,@(loop :for (s . f) :in forms
              :if (stringp s) :collect `((string= ,test ,s) ,@f)
	      :else :if (consp s)
		        :append (loop for element :in s
				       :if (stringp element)
					   :collect `((string= ,test ,element) ,@f)
			               :else :collect `((eql ,test ,s) ,@f))
	      :else :if (string= s 'otherwise) :collect `(t ,@f)
              :else :collect `((eql ,test ,s) ,@f))))))

(declaim (inline expand-match-branch))
(defun expand-match-branch (str block patterns forms)
  "Helper function of the match macro."
  (case patterns
    ((t 'otherwise) `(return-from ,block (progn ,@forms)))
    (t (loop with regex = '("^")
             and vars = '()
             and ind = 0
             for x in patterns
             do (cond ((stringp x)
                       (push x regex))
                      ((symbolp x)
                       (push "(.*?)" regex)
                       (push (list x ind) vars)
                       (incf ind))
                      (t (error "only symbol and string allowed in patterns")))
             finally (push "$" regex)
             finally (setf vars (reverse vars))
             finally (return (let ((whole-str (gensym))
                                   (regs (gensym)))
                               `(multiple-value-bind (,whole-str ,regs)
                                    (cl-ppcre:scan-to-strings
                                     ,(apply #'str:concat (reverse regex))
                                     ,str)
                                  (declare (ignore ,whole-str)
                                           ((or null (simple-array string (*))) ,regs))
                                  (when ,regs
                                    (let ,(loop for (v ind) in vars
                                                unless (string= (symbol-name v) "_")
                                                  collect v)
                                      ,@(loop for (v ind) in vars
                                              unless (string= (symbol-name v) "_")
                                                collect `(setf ,v (aref ,regs ,ind)))
                                      (return-from ,block
                                        (progn ,@forms)))))))))))

(defmacro match (str &body match-branches)
  "A COND-like macro to match substrings and bind variables to matches. Regular expressions are allowed for matches.

  _ is a placeholder that is ignored.

  THIS MACRO IS EXPERIMENTAL and might break in future releases.

  Example:

  (str:match \"a 1 b 2 d\"
    ((\"a \" x \" b \" y \" d\") ;; => matched
     (+ (parse-integer x) (parse-integer y)))
    (t
     'default-but-not-for-this-case)) ;; default branch
  ;; => 3

  (str:match \"a 1 b c d\"
    ((\"a 2 b\" _ \"d\") ;; => not matched
     (print \"pass\"))
    ((\"a \" _ \" b c d\") ;; => matched
     \"here we go\")
    (t 'default-but-not-for-this-case)) ;; default branch
  ;; => \"here we go\"

  Match with regexs:

  (str:match \"123 hello 456\"
    ((\"\\d+\" s \"\\d+\")
     s)
  (t \"nothing\"))
  ;; => \" hello \"
  "
  (let ((block-sym (gensym)))
    `(block ,block-sym
       ,@(loop for statement in match-branches
               collect (expand-match-branch
                        str
                        block-sym
                        (nth 0 statement)
                        (cdr statement))))))

(declaim (inline s-first)
         (ftype (function ((or string null)) (or string null)) s-first))
(defun s-first (s)
  "Return the first substring of `s'."
  (etypecase s
    (null nil)
    ((string 0) "")
    (t (subseq s 0 1))))

(declaim (inline s-last)
         (ftype (function ((or string null)) (or string null)) s-last))
(defun s-last (s)
  "Return the last substring of `s'."
  (etypecase s
    (null nil)
    ((string 0) "")
    (t (substring (1- (length s)) t s))))

(declaim (inline s-rest)
         (ftype (function ((or string null)) (or string null)) s-rest))
(defun s-rest (s)
  "Return the rest substring of `s'."
  (etypecase s
    (null nil)
    ((string 0) "")
    (t (subseq s 1))))

(declaim (ftype (function (integer (or string null)) (or string null)) s-nth))
(defun s-nth (n s)
  "Return the nth substring of `s'.

   You could also use
   (string (elt \"test\" 1))
   ;; => \"e\""
  (cond ((null s) nil)
        ((or (emptyp s) (minusp n)) "")
        ((= n 0) (s-first s))
        (t (s-nth (1- n) (s-rest s)))))

(declaim (inline s-assoc-value))
(defun s-assoc-value (alist key)
  "Return the value of a cons cell in `alist' with key `key', tested
with `string-equal' (case-insensitive).
  The second return value is the cons cell."
  (let ((cons (assoc key alist :test #'string-equal)))
    (values (cdr cons) cons)))

(declaim (inline s-member))
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

(defun count-substring (substring s &key (start 0) (end nil)
                                      (ignore-case *ignore-case*))
  "Return the non-overlapping occurrences of `substring' in `s'.
  You could also count only the ocurrencies between `start' and `end'.
  When `ignore-case` is T, ignore case when counting matches.

  Examples:
  (count-substring \"abc\" \"abcxabcxabc\")
  ;; => 3

  (count-substring \"abc\" \"abcxabcxabc\" :start 3 :end 7)
  ;; => 1"
  (unless (or (null s)
              (null substring)
              (emptyp substring))
    (loop :with test := (if ignore-case #'string-equal #'string=)
       :with substring-length := (length substring)
       :for position := (search substring s :test test :start2 start :end2 end)
       :then (search substring s :test test :start2 (+ position substring-length) :end2 end)
       :while (not (null position))
       :summing 1)))

;;; Case

;; Small wrappers around built-ins that return nil when the argument is nil.

(declaim (inline downcase))
(defun downcase (s)
  "Return the lowercase version of `s'.
  Calls the built-in `string-downcase', but returns nil if `s' is
  nil (instead of the string \"nil\").

  Examples:
  (downcase \"Foo fooF\") => \"foo foof\"
  (downcase :foo{foo.f) => \"foo{foo.f\"
  (downcase #\F) => \"f\"
  (downcase nil) => nil"
  (when s
    (string-downcase s)))

(declaim (inline upcase))
(defun upcase (s)
  "Return the uppercase version of `s'.
  Call the built-in `string-upcase', but return nil if `s' is
  nil (instead of the string \"NIL\").

  Examples:
  (upcase \"Foo fooF\") => \"FOO FOOF\"
  (upcase :foo{foo.f) => \"FOO{FOO.F\"
  (upcase #\f) => \"F\"
  (upcase nil) => nil"
  (when s
    (string-upcase s)))

(declaim (inline capitalize))
(defun capitalize (s)
  "Return the capitalized version of `s'.
  Calls the built-in `string-capitalize', but returns nil if `s' is
  nil (instead of the string \"Nil\").

  Examples:
  (capitalize \"Foo fooF\") => \"Foo fooF\"
  (capitalize :foo{foo.f) => \"Foo{Foo.F\"
  (capitalize #\f) => \"F\"
  (capitalize nil)  => nil"
  (when s
    (string-capitalize s)))

;; Wrappers around cl-change-case functions that coerce the argument into a string
;; and return nil when the argument is nil.

(declaim (inline no-case))
(defun no-case (s &key (replacement " "))
  "Transform `s' to lower case space delimited. Use REPLACEMENT as delimiter.

  Examples:
  (no-case \"Foo fooF\" :replacement \",\") => \"foo,foo,f\"
  (no-case 'foo{foo.f) => \"foo foo f\"
  (no-case #\F) => \"f\"
  (no-case nil) => nil"
  (when s
    (cl-change-case:no-case (string s) :replacement replacement)))

(declaim (inline camel-case))
(defun camel-case (s &key merge-numbers)
  "Transform `s' to camelCase.
Dot-separated numbers like 1.2.3 will be replaced by underscores 1_2_3
unless MERGE-NUMBERS is non-nil.

  Examples:
  (camel-case \"Foo fooF\") => \"fooFooF\"
  (camel-case (quote foo{foo.f)) => \"fooFooF\"
  (camel-case #\F) => \"f\"
  (camel-case nil) => nil"
  (when s
    (cl-change-case:camel-case (string s) :merge-numbers merge-numbers)))

(declaim (inline dot-case))
(defun dot-case (s)
  "Transform `s' to dot.case.

  Examples:
  (dot-case \"Foo fooF\") => \"foo.foo.f\"
  (dot-case :foo{foo-f) => \"foo.foo.f\"
  (dot-case #\F) => \"f\"
  (dot-case nil) => nil"
  (when s
    (cl-change-case:dot-case (string s))))

(declaim (inline header-case))
(defun header-case (s)
  "Transform `s' to Header-Case.

  Examples:
  (header-case \"Foo fooF\") => \"Foo-Foo-F\"
  (header-case 'foo{foo.f) => \"Foo-Foo-F\"
  (header-case #\f) => \"F\"
  (header-case nil) => nil"
  (when s
    (cl-change-case:header-case (string s))))

(declaim (inline param-case))
(defun param-case (s)
  "Transform `s' to param-case.

  Examples:
  (param-case \"Foo fooF\") => \"foo-foo-f\"
  (param-case (quote foo{foo.f)) => \"foo-foo-f\"
  (param-case #\F) => \"f\"
  (param-case nil) => nil"
  (when s
    (cl-change-case:param-case (string s))))

(declaim (inline pascal-case))
(defun pascal-case (s)
  "Transform `s' to Pascal Case

  Examples:
  (pascal-case \"Foo fooF\") => \"FooFooF\"
  (pascal-case :foo{foo.f) => \"FooFooF\"
  (pascal-case #\f) => \"F\"
  (pascal-case nil) => nil"
  (when s
    (cl-change-case:pascal-case (string s))))

(declaim (inline path-case))
(defun path-case (s)
  "Transform `s' to path/case

  Examples:
  (path-case \"Foo fooF\") => \"foo/foo/f\"
  (path-case :foo{foo.f) => \"foo/foo/f\"
  (path-case #\F) => \"f\"
  (path-case nil) => nil"
  (when s
    (cl-change-case:path-case (string s))))

(declaim (inline sentence-case))
(defun sentence-case (s)
  "Transform `s' to Sentence case

  Examples:
  (sentence-case \"Foo.fooF\") => \"Foo foo f\"
  (sentence-case (quote foo{foo.f)) => \"Foo foo f\"
  (sentence-case #\f) => \"F\"
  (sentence-case nil) => nil"
  (when s
    (cl-change-case:sentence-case (string s))))

(declaim (inline snake-case))
(defun snake-case (s)
  "Transform `s' to snake_case

  Examples:
  (snake-case \"Foo fooF\") => \"foo_foo_f\"
  (snake-case 'foo{foo.f) => \"foo_foo_f\"
  (snake-case #\f) => \"f\"
  (snake-case nil) => nil"
  (when s
    (cl-change-case:snake-case (string s))))

(declaim (inline swap-case))
(defun swap-case (s)
  "Reverse case for each character in `s'.

  Examples:
  (swap-case \"Foo fooF\") => \"fOO FOOf\"
  (swap-case :FOO{FOO.F) => \"foo{foo.f\"
  (swap-case #\f) => \"F\"
  (swap-case nil) => nil"
  (when s
    (cl-change-case:swap-case (string s))))

(declaim (inline title-case))
(defun title-case (s)
  "Transform `s' to Title Case

  Examples:
  (title-case \"Foo fooF\") => \"Foo Foo F\"
  (title-case :foo{foo.f) => \"Foo Foo F\"
  (title-case #\f) => \"F\"
  (title-case nil) => nil"
  (when s
    (cl-change-case:title-case (string s))))

(declaim (inline constant-case))
(defun constant-case (s)
  "Transform `s' to CONSTANT_CASE.

  (constant-case \"Foo fooF\") => \"FOO_FOO_F\"
  (constant-case :foo{foo.f) => \"FOO_FOO_F\"
  (constant-case #\f) => \"F\"
  (constant-case nil) => nil"
  (when s
    (cl-change-case:constant-case (string s))))

;;; Case predicates.

(declaim (inline alphanump))
(defun alphanump (s)
  "Return t if `s' contains at least one character and all characters are alphanumeric.
  See also `lettersnump' which also works on unicode letters."
  (not (ppcre:scan "[^a-zA-Z0-9]" s)))

(declaim (inline alphap))
(defun alphap (s)
  "Return t if `s' contains at least one character and all characters are alpha (in [a-zA-Z]).
  See also `lettersp', which checks for unicode letters."
  (not (ppcre:scan "[^a-zA-Z]" s))
  ;; TODO: this regexp accepts é and ß: in lettersp like cuerdas ?
  ;; and like in python, so definitely yes.
  ;; (ppcre:scan-to-strings "^\\p{L}+$" s)
  )

(declaim (inline lettersp))
(defun lettersp (s)
  "Return t if `s' contains only letters (including unicode letters).

   (alphap \"éß\") ;; => nil
   (lettersp \"éß\") ;; => t"
  (unless (ppcre:scan "\\P{L}" s)
    t))

(declaim (inline lettersnump))
(defun lettersnump (s)
  "Return t if `s' contains only letters (including unicode letters) and digits."
  (unless (ppcre:scan "[^\\p{L}a-zA-Z0-9]" s)
    t))

(declaim (inline digitp))
(defun digitp (s)
  "Return t if `s' contains at least one character and all characters are numerical."
  (unless (emptyp s)
    ;; regex ? Check sign and exponents.
    (every #'digit-char-p s)))

;; An alias for digitp
(setf (fdefinition 'numericp) #'digitp)

(declaim (inline has-alphanum-p)
         (ftype (function (t) boolean) has-alphanum-p))
(defun has-alphanum-p (s)
  "Return t if `s' has at least one alphanumeric character."
  (unless (emptyp s)
    (some (lambda (char)
            (alphanumericp char))
          s)))

(declaim (inline has-alpha-p)
         (ftype (function (t) boolean) has-alpha-p))
(defun has-alpha-p (s)
  "Return t if `s' has at least one alpha character ([a-zA-Z])."
  (when (ppcre:scan "[a-zA-Z]" s)
    t))

(declaim (inline has-letters-p)
         (ftype (function (t) boolean) has-letters-p))
(defun has-letters-p (s)
  "Return t if `s' contains at least one letter (considering unicode, not only alpha characters)."
  (when (ppcre:scan "\\p{L}" s)
    t))

(declaim (inline ascii-char-p)
         (ftype (function (t) boolean) ascii-char-p))
(defun ascii-char-p (char)
  "Return t if `char' is an ASCII char (its char code is below 128)."
  ;; Inspired by Serapeum
  (when (and (< (char-code char) 128)
             char)
    t))

(declaim (inline ascii-p)
         (ftype (function (t) boolean) ascii-p))
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

(declaim (inline downcasep))
(defun downcasep (s)
  "Return t if all alphabetical characters of `s' are lowercase, and `s' contains at least one letter."
  (when (characterp s)
    (return-from downcasep (lower-case-p s)))

  (assert (or (null s)
              (stringp s)))
  (if (has-letters-p s)
      (every (lambda (char)
               (if (alpha-char-p char)
                   (lower-case-p char)
                   t))
             s)))

(declaim (inline upcasep))
(defun upcasep (s)
  "Return t if all alphabetical characters of `s' are uppercase."
  (when (characterp s)
    (return-from upcasep (upper-case-p s)))

  (assert (or (null s)
              (stringp s)))
  (when (has-letters-p s)
      (every (lambda (char)
               (if (alpha-char-p char)
                   (upper-case-p char)
                   t))
             s)))

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

;; "deprecated" function alias
(setf (fdefinition 'prune)        #'shorten
      (fdefinition 'common-prefix) #'prefix
      (fdefinition 'empty?)       #'emptyp
      (fdefinition 'blank?)       #'blankp
      (fdefinition 'starts-with?) #'starts-with-p
      (fdefinition 'ends-with?)   #'ends-with-p
      (fdefinition 'contains?)    #'containsp
      (fdefinition 'prefix?)      #'prefixp
      (fdefinition 'suffix?)      #'suffixp
      (fdefinition 'alphanum?)    #'alphanump
      (fdefinition 'alpha?)       #'alphap
      (fdefinition 'letters?)     #'lettersp
      (fdefinition 'digit?)       #'digitp
      (fdefinition 'numeric?)     #'digitp
      (fdefinition 'downcase?)    #'downcasep
      (fdefinition 'upcase?)      #'upcasep)
