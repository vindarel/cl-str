(in-package :cl-user)
(defpackage test-str
  (:use #:cl
        #:str)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals
                #:def-fixture
                #:with-fixture)
  (:export #:str
           #:replace-functions
           #:lengthen-functions
           #:ensure-functions
           #:pad-functions
           #:substring-functions
           #:list-functions
           #:from-list-to-string
           #:from-list-to-list
           #:from-string-to-list
           #:predicates
           #:case-functions
           #:miscellaneous))

;;; Test for cl-str.
;;;
;;; To run the test at point when compiling it with C-c C-c, set this:

#+(or)
(setf fiveam:*run-test-when-defined* t)


(in-package :test-str)

(def-suite str
  :description "Top level test suite")

(def-fixture ignore-case ()
  (let ((*ignore-case* t))
    (&body)))

(def-fixture omit-nulls ()
  (let ((*omit-nulls* t))
    (&body)))

(def-suite replace-functions
  :in str
  :description "Functions that replace specific elements from strings.")
(in-suite replace-functions)

(test replace-first
  (is (string= "fooaa" (replace-first "aa" "oo" "faaaa")))
  (is (string= "frobfoo bar" (replace-first "fo+" "frob" "foofoo bar" :regex t)))
  (is (string= "frob bar FOO" (replace-first "(?i)fo+" "frob" "FOO bar FOO" :regex t))))

(test replace-all
  (is (string= "foo" (replace-all "a" "o" "faa")))
  (is (string= "foo" (replace-all "^a" "o" "fo^a")))
  (is (string= "foo" (replace-all "^aa+" "o" "fo^aa+")))
  (is (string= "foo'\\'bar" (replace-all "+" "'\\'" "foo+bar"))
      "Edge case with a double backslash and a single quote.")
  (is (string= "frobfrob bar" (replace-all "fo+" "frob" "foofoo bar" :regex t)))
  (is (string= "frob bar frob" (replace-all "(?i)fo+" "frob" "FOO bar FOO" :regex t))))

(test replace-using
  (is (string= "foo" (replace-using (list "a" "o") "faa")))
  (is (string= "fooAA" (replace-using (list "a" "o") "faaAA")))
  (is (string= "fooBB" (replace-using (list "a" "o" "A" "B") "faaAA")))
  (signals error (replace-using (list "a") "faa"))
  (signals error (replace-using (list "a" "o" "A") "faaAA"))
  (signals error (replace-using (list 1 "C") "faaAA"))
  (signals error (replace-using (list "f" 'test) "faaAA"))
  (is (string= "faa" (replace-using nil "faa")))
  (is (string= "frob Bobr"
               (replace-using (list "fo+" "frob" "ba+" "Bob") "foo bar" :regex t))))

(def-suite lengthen-functions
  :in str
  :description "Functions that add elements to strings.")
(in-suite lengthen-functions)

(test concat
  (is (string= "foo" (concat "f" "o" "o")))
  (is (string= "" (concat))))

(test insert
  (is (string= "hello" (insert "o" 4 "hell")))
  (is (string= "hello" (insert "h" 0 "ello")))
  (is (string= "hell" (insert "l" 200 "hell")) "large index")
  (is (string= "hell" (insert "l" -2 "hell")) "negative index")
  (is (string= "hell" (insert nil 2 "hell")) "insert nil: do nothing")
  (is (string= "hell" (insert "l" nil "hell")) "index nil: do nothing")
  (is (string= nil (insert "l" 2 nil)) "s nil: nil")
  (is (string= "hello" (insert #\o 4 "hell")) "with a char"))

(def-suite ensure-functions
  :in lengthen-functions
  :description "Functions that ensure elements before and/or after a string.")
(in-suite ensure-functions)

(test ensure
  (is (string= "-foo+" (ensure "foo" :prefix "-" :suffix "+"))
      "ensure with prefix and suffix")
  (is (string= "foo" (ensure "foo")) "ensure with no params")
  (is (string= "foo" (ensure "foo" :prefix nil :suffix nil :wrapped-in nil))
      "ensure with params to nil")
  (is (string= "/foo/" (ensure "foo" :wrapped-in "" :prefix "/" :suffix "/"))
      "ensure with wrapped-in blank and prefix and suffix."))

(test ensure-prefix
  (is (string= "/abc" (ensure-prefix "/" "/abc")) "default case: existing prefix.")
  (is (string= "/abc" (ensure-prefix "/" "abc")) "default case: add prefix.")
  (is (string= "/" (ensure-prefix "/" "")) "blank string: add prefix")
  (is (string= "/" (ensure-prefix #\/ "")) "with a char")
  (is (string= "" (ensure-prefix "" "")) "blank strings")
  (is (string= "" (ensure-prefix nil "")) "prefix is nil, we want s")
  (is (string= nil (ensure-prefix nil nil)) "prefix and s are nil")
  (is (string= nil (ensure-prefix "/" nil)) "s is nil")
  (is (string= "///abc" (ensure-prefix "/" "///abc")) "lots of slashes, but that's ok"))

(test ensure-suffix
  (is (string= "/abc/" (ensure-suffix "/" "/abc/")) "default case")
  (is (string= "abc/" (ensure-suffix "/" "abc")) "default case 2")
  (is (string= "/" (ensure-suffix "/" "")) "default case void string")
  (is (string= "/" (ensure-suffix #\/ "")) "with a char")
  (is (string= "" (ensure-suffix "" "")) "void strings")
  (is (string= "foo" (ensure-suffix nil "foo")) "prefix is nil, we want s")
  (is (string= "abc///" (ensure-suffix "/" "abc///")) "lots of slashes, but that's ok"))

(test ensure-wrapped-in
  (is (string= "/abc/" (ensure-wrapped-in "/" "/abc/")) "default case")
  (is (string= "/abc/" (ensure-wrapped-in "/" "abc")) "default case 2")
  (is (string= "/" (ensure-wrapped-in "/" "")) "default case void string")
  (is (string= "/" (ensure-wrapped-in #\/ "")) "with a char")
  (is (string= "" (ensure-wrapped-in "" "")) "blank strings")
  (is (string= "" (ensure-wrapped-in nil "")) "prefix is nil, we want s")
  ;; The following line is different that the original behaviour of starts-with-p:
  (is (string= nil (ensure-wrapped-in "" nil)) "blank prefix, s is nil")
  ;; (starts-with-p "" nil) ;; => T but below, we expect NIL.
  (is (string= nil (ensure-wrapped-in nil nil)) "nils")
  (is (string= "/abc///" (ensure-wrapped-in "/" "abc///"))
      "lots of slashes, but that's ok"))

(def-suite pad-functions
  :in lengthen-functions
   :description "Add padding elements before and/or after a string.")
(in-suite pad-functions)

(test pad
  (is (string= "foo       " (pad 10 "foo"))
      "pad adds spaces on the right by default.")
  (is (string= "       foo" (pad 10 "foo" :pad-side :left))
      "pad with pad-site :left")
  (is (string= "   foo    " (pad 10 "foo" :pad-side :center))
      "pad with pad-side :center")
  (is (string= "foo+++++++" (pad 10 "foo" :pad-char #\+))
      "pad with a custom padding character.")
  (is (string= "foo" (pad -1 "foo"))
      "pad with a short length returns the string."))

(test pad-right
  (is (string= (pad 10 "foo") (pad-right 10 "foo"))
      "pad-right is equivalent to pad"))

(test pad-left
  (is (string= (pad 10 "foo" :pad-side :left) (pad-left 10 "foo"))
      "pad-left"))

(test pad-center
  (is (string= (pad 10 "foo" :pad-side :center) (pad-center 10 "foo"))
      "pad-center"))

(def-suite substring-functions
  :in str
  :description "Functions that remove elements from a string.")
(in-suite substring-functions)

(test trim
  (is (string= "rst" (trim "  rst  ")))
  (is (string= nil (trim nil)))
  (is (string= "rst" (trim "drste" :char-bag "de")))
  (is (string= "rst" (trim "　　rst　　　"))))

(test trim-right
  (is (string= " rst" (trim-right " rst   ")))
  (is (string= nil (trim-right nil)))
  (is (string= " rst" (trim-right " rstbc" :char-bag (list #\b #\c)))))

(test trim-left
  (is (string= "rst " (trim-left "   rst ")))
  (is (string= nil (trim-left nil)))
  (is (string= "rst " (trim-left "abrst " :char-bag "ab"))))

(test collapse-whitespaces
  (is (string= "foo bar baz" (collapse-whitespaces "foo  bar


  baz"))))

(test substring
  (is (string= "abcd" (substring 0 4 "abcd")) "normal case")
  (is (string= "ab" (substring 0 2 "abcd")) "normal case substing")
  (is (string= "bc" (substring 1 3 "abcd")) "normal case substing middle")
  (is (string= "" (substring 4 4 "abcd")) "normal case")
  (is (string= "" (substring 0 0 "abcd")) "normal case")
  (is (string= "d" (substring 3 4 "abcd")) "normal case")
  (is (string= "abcd" (substring 0 t "abcd")) "end is t")
  (is (string= "abcd" (substring 0 nil "abcd")) "end is nil")
  (is (string= "abcd" (substring 0 100 "abcd")) "end is too large")
  (is (string= "abc" (substring 0 -1 "abcd")) "end is negative")
  (is (string= "b" (substring 1 -2 "abcd")) "end is negative")
  (is (string= "" (substring 2 1 "abcd")) "start is bigger than end")
  (is (string= "" (substring 0 -100 "abcd")) "end is too low")
  (is (string= "" (substring 100 1 "abcd")) "start is too big")
  (is (string= "abcd" (substring -100 4 "abcd")) "start is too low")
  (is (string= "abcd" (substring -100 100 "abcd")) "start and end are too low and big")
  (is (string= "" (substring 100 -100 "abcd")) "start and end are too big and low"))

(test shorten
  (is (string= "hello..." (shorten 8 "hello foobar"))
      "default case.")
  (is (string= "foo" (shorten 10 "foo"))
      "long, no change")
  (is (string= "..." (shorten 1 "foo"))
      "short, only ellipsis")
  (is (string= "-" (shorten 1 "foo" :ellipsis "-"))
      "custom ellipsis")
  (is (string= "-"
               (let ((str:*ellipsis* "-"))
                 (shorten 1 "foo")))
      "custom ellipsis with let")
  (is (string= "hello-" (shorten 6 "hello foobar" :ellipsis "-"))
      "shorter ellipsis")
  (is (string= "foo" (shorten nil "foo"))
      "length is nil")
  (is (string= "..." (shorten -1 "foo"))
      "length is negative")
  (is (string= nil (shorten 10 nil))
      "s is nil"))

(test s-first
  (is (string= nil (s-first nil)))
  (is (string= "f" (s-first "foobar")))
  (is (string= "" (s-first ""))))

(test s-last
  (is (string= nil (s-last nil)))
  (is (string= "b" (s-last "b")))
  (is (string= "r" (s-last "bar")))
  (is (string= "" (s-last ""))))

(test s-rest
  (is (string= nil (s-rest nil)))
  (is (string= "oobar" (s-rest "foobar")))
  (is (string= "" (s-rest ""))))

(test s-nth
  (is (string= nil (s-nth 1 nil)))
  (is (string= "b" (s-nth 3 "foobar")))
  (is (string= "" (s-nth -1 "foobar")))
  (is (string= "" (s-nth 6 "foobar")))
  (is (string= "" (s-nth 3 ""))))

(def-suite list-functions
  :in str
  :description "Functions that work with lists.")
(in-suite list-functions)

(def-suite from-list-to-string
  :in list-functions
  :description "Test for functions with lists as input.")
(in-suite from-list-to-string)

(test join
  (is (string= "foo bar baz" (join " " '("foo" "bar" "baz"))))
  (is (string= "foo+++bar+++baz" (join "+++" '("foo" "bar" "baz"))))
  (is (string= "foo~bar" (join "~" '("foo" "bar"))))
  (is (string= "foo~~~bar" (join "~" '("foo~" "~bar"))))
  (is (string= "foo,bar" (join #\, '("foo" "bar"))))
  (is (string= "" (join nil nil)))
  (is (string= "abcde" (join nil '("a" "b" "c" "d" "e")))))

(test unwords
  (is (string= "" (unwords nil)))
  (is (string= "" (unwords '())))
  (is (string= "" (unwords '(""))))
  (is (string= "foo" (unwords '("foo"))))
  (is (string= "foo bar baz" (unwords '("foo bar baz")))))

(test unlines
  (is (string= "" (unlines nil)))
  (is (string= "" (unlines '(""))))
  (is (string= "
" (unlines '("" ""))))
  (is (string= "1
2
" (unlines '("1" "2" "")))))

(test paragraphs
  (let ((s "abc


def
")
        ;; only one newline and no trailing newline
        (simple-string "abc

def"))
    (is (string= "abc" (first (paragraphs s))) "default case")
    (is (string= "def" (second (paragraphs s))) "trim other paragraphs")
    (is (not (string= s (unparagraphs (paragraphs s)))))
    ;; simple-string:
    (is (string= simple-string (unparagraphs (paragraphs simple-string)))))
  )

(test prefix
  (is (string= "foo" (prefix '("foobar" "footeam"))) "default case")
  (is (string= "" (prefix '("foobar" "barfoo"))) "no common prefix")
  (is (string= "" (prefix '("foobar" ""))) "with empty string")
  (is (string= "" (prefix '("foobar" nil))) "with a nil")
  (is (string= nil (prefix '())) "with void list"))

(test suffix
  (is (string= "bar" (suffix '("foobar" "teambar")))  "default case")
  (is (string= "" (suffix '("foobar" "barfoo"))) "no common suffix")
  (is (string= "" (suffix '("foobar" ""))) "with empty string")
  (is (string= "" (suffix '("foobar" nil))) "with a nil")
  (is (string= nil (suffix '())) "with void list"))

(def-suite from-list-to-list
  :in list-functions
  :description "Test for functions with lists as input.")
(in-suite from-list-to-list)

(test add-prefix
  (is (equalp '("foobar" "footeam") (add-prefix '("bar" "team") "foo")) "default case")
  (is (equalp '("foobar" "foo") (add-prefix '("bar" nil) "foo")) "with a nil")
  (is (equalp '() (add-prefix '() "foo")) "with void list"))

(test add-suffix
  (is (equalp '("foobar" "teambar") (add-suffix '("foo" "team") "bar")) "default case")
  (is (equalp '("foobar" "bar") (add-suffix '("foo" nil) "bar")) "with a nil")
  (is (equalp '() (add-suffix '() "foo")) "with void list"))

(def-suite from-string-to-list
  :in list-functions
  :description "Functions that output lists.")
(in-suite from-string-to-list)

(test words
  (is (string= nil (words nil)))
  (is (string= nil (words "")))
  (is (equalp '("foo") (words "foo")))
  (is (equalp '("foo" "bar") (words "foo bar")))
  (is (equalp '("foo" "bar") (words "  foo   bar   ")))
  (is (equalp '("foo" "bar  ") (words " foo bar  " :limit 2)))
  (is (equalp '("foo" "bar baz ") (words " foo bar baz " :limit 2)))
  (is (equalp '("foo" "bar" "baz" "?") (words "  foo bar  baz ?")))
  (is (equalp '("foo" "bar" "baz" "?") (words "  foo
 bar  baz ?"))))

(test split
  (is (equalp '("foo" "bar") (split " " "foo bar")))
  (is (equalp '("foo" "bar") (split "+" "foo+bar")) "separator is a regexp")
  (is (equalp '("foo" "" "bar") (split "+" "foo++bar")))
  (is (equalp '("foo" "bar+car+dar") (split "+" "foo+bar+car+dar" :limit 2)))
  (is (equalp '("foo" "bar") (split "+" "foo+bar")))
  (is (equalp '("foo" "bar") (split "x" "fooxbar")) "split with string x")
  (is (equalp '("foo" "bar") (split #\x "fooxbar")) "split with character")
  (is (equalp '("fooxbarx") (split "xx" "fooxbarx")) "split with xx, end in x")
  (is (equalp '("foo" "barx") (split "xx" "fooxxbarx")) "split with xx")
  (is (equalp '("fooxbar" "x") (split "xx" "fooxbarxxx")) "split with xx, end in xxx")
  (is (equalp '("foo" "bar") (split "(*)" "foo(*)bar")))
  (is (equalp '("foo" "bar" "") (split "NO" "fooNObarNO"))
      "separator at the end (cl-ppcre doesn't return an empty string), but we do.")
  (is (equalp '("foo" "bar" "") (split "NO" "fooNObarNO" :limit 10))
      "but cl-ppcre does return trailing empty strings if limit is provided")
  (is (equalp '("foo" "bar") (split "+" "foo+++bar++++" :omit-nulls t))
      "omit-nulls argument")
  (is (equalp '("foo" "   ") (split "+" "foo+++   ++++" :omit-nulls t))
      "omit-nulls and blanks")
  (with-fixture omit-nulls ()
    (is (equalp '("foo" "bar") (split "+" "foo+++bar++++"))
        "omit-nulls argument")
    (is (equalp '("foo" "   ") (split "+" "foo+++   ++++"))
        "omit-nulls and blanks"))
  (is (equalp '("foo" "bar") (split "," "foo,bar")))
  (is (equalp '("foo" "ABbar") (split "ABAB" "fooABABABbar")))
  (is (equalp '("foo" "bar" "baz") (split "[,|;]" "foo,bar;baz" :regex t)))
  (is (equalp '("foo" "bar" "" "" "baz") (split "[,|;+]" "foo,bar;;;baz" :regex t))))

(test rsplit
  (is (equalp '("foo" "bar") (rsplit " " "foo bar")))
  (is (equalp '("foo" "bar") (rsplit #\, "foo,bar")))
  (is (equalp '("com.foo" "Bar") (rsplit "." "com.foo.Bar" :limit 2)))
  (is (equalp '("/var/log" "mail.log") (rsplit "/" "/var/log/mail.log" :limit 2)))
  (is (equalp '("/var" "log" "mail.log") (rsplit "/" "/var/log/mail.log" :limit 3)))
  (is (equalp '("" "var" "log" "mail.log") (rsplit "/" "/var/log/mail.log" :limit 4)))
  (is (equalp '("foo" "bar") (rsplit "LONG" "fooLONGbar")))
  (is (equalp '("foo" "bar" "") (rsplit "LONG" "fooLONGbarLONG" :limit 3)))
  (is (equalp '("fooAB" "bar") (rsplit "ABAB" "fooABABABbar")))
  (is (equalp '("foo" "bar" "" "" "baz") (rsplit "[,|;+]" "foo,bar;;;baz" :regex t))))

(test lines
  (is (string= nil (lines nil)))
  (is (string= nil (lines "")))
  (is (equalp '("") (lines "
")))
  (is (equalp '("1" "2" " 3") (lines "1
2
 3")))
  (is (equalp '("1" "2" " 3") (lines "1
2
 3
")))
  (is (equalp '("1" "2" "" "3") (lines "1
2

3")))
  (is (equalp '("1" "2" "3") (lines "1
2


3" :omit-nulls t))))

(def-suite predicates
  :in str)
(in-suite predicates)

(test emptyp
  (is (emptyp nil))
  (is (emptyp ""))
  (is (not (emptyp " "))))

(test non-empty-string-p
  (is (not (non-empty-string-p 8)))
  (is (not (non-empty-string-p nil)))
  (is (not (non-empty-string-p #\x)))
  (is (not (non-empty-string-p "")))
  (is (non-empty-string-p "  "))
  (is (non-empty-string-p "foo"))
  (is (non-empty-string-p "8")))

(test non-blank-string-p
  (is (not (non-blank-string-p 8)))
  (is (not (non-blank-string-p "")))
  (is (not (non-blank-string-p "  ")))
  (is (not (non-blank-string-p "

  ")))
  (is (non-blank-string-p "foo"))
  (is (non-blank-string-p "8")))

(test blankp
  (is (blankp "  "))
  (is (blankp "  "))
  (is (not (blankp "   rst "))))

(test containsp
  (is (containsp "foo" "blafoobar") "default")
  (is (not (containsp "foo" "")) "with no string")
  (is (not (containsp "" nil)) "a blank substring in a nil str")
  (is (not (containsp nil "")) "a nil substring in a blank str")
  (is (not (containsp "foo" nil)) "with string nil")
  (is (not (containsp "Foo" "blafoobar")) "with case")
  (is (containsp "Foo" "blafoobar" :ignore-case t) "ignore case")
  (is (containsp "Foo" "blafoobar" :ignore-case t) "containsp alias")
  (with-fixture ignore-case ()
    (is (containsp "Foo" "blafoobar") "ignore case")))

(test s-member
  (is (s-member '("bar" "foo") "foo")
      "downcase")
  (is (s-member '("bar" "FOO") "FOO")
      "upcase")
  (is (not (s-member '("bar" "foo") "FOO"))
      "significant case by default")
  (is (s-member '("bar" "foo") "FOO" :ignore-case t)
      "with ignore-case")
  (with-fixture ignore-case ()
    (is (s-member '("bar" "foo") "FOO")
        "with *ignore-case*")
    (is (s-member '("bar" "foo") "FOO" :test #'string-equal)
        "with :test")))

(test lettersp
  (is (lettersp "éß") "letters with accents and ß")
  (is (not (lettersp " e é,")) "no letters")
  (is (not (lettersp "éß
")) "not lettersp with newline"))

(test lettersnump
  (is (lettersnump "éß3") "lettersnump letters with accents and ß and a number")
  (is (not (lettersnump "éß3
")) "not lettersnump with newline"))

(test has-letters-p 
  (is (has-letters-p " e é ß") "has-letters-p default")
  (is (not (has-letters-p " +,-")) "has-letters-p default nil")
  (is (lettersnump "abcéß123") "lettersnump default"))

(test ascii-p
  (is (ascii-p #\a) "with a character")
  (is (ascii-p "abc") "with a string")
  (is (not (ascii-p "abcéèö")) "bad string")
  (is (not (ascii-p #\é)) "bad character")
  (is (not (ascii-p nil)) "with nil"))

(test starts-with-p
  (is (starts-with-p "foo" "foobar") "default case")
  (is (starts-with-p "" "foo") "with blank start")
  (is (not (starts-with-p "rs" "")) "with blank s")
  (is (not (starts-with-p nil "")) "prefix is nil")
  ;; (is (string= (starts-with? "" nil) t) "s is nil: what do we want?")  ;; XXX: fix?
  (is (not (starts-with-p "foobar" "foo")) "with shorter s")
  (is (starts-with-p "" "") "with everything blank")
  (is (not (starts-with-p "FOO" "foobar")) "don't ignore case")
  (is (starts-with-p "f" "foo") "starts-with-p alias")
  (is (starts-with-p "FOO" "foobar" :ignore-case t) "ignore case")
  (with-fixture ignore-case ()
    (is (starts-with-p "FOO" "foobar") "ignore case")))

(test ends-with-p
  (is (ends-with-p "bar" "foobar") "default case")
  (is (ends-with-p "bar" "foobar") "ends-with-p alias")
  (is (not (ends-with-p "BAR" "foobar")) "don't ignore case")
  (is (ends-with-p "BAR" "foobar" :ignore-case t) "ignore case")
  (with-fixture ignore-case ()
    (is (ends-with-p "BAR" "foobar") "ignore case")))

(test prefixp
  (is (string= "foo" (prefixp '("foobar" "footeam") "foo")) "default case")
  (is (string= "f" (prefixp '("foobar" "footeam") "f")) "smaller prefix")
  (is (string= nil (prefixp '("foobar" "barfoo") "x")) "not a common prefix")
  (is (string= "" (prefixp '("foobar" "barfoo") "")) "prefix is a void string")
  (is (string= "" (prefixp '("foobar" "") "")) "with empty string")
  (is (string= "" (prefixp '("foobar" nil) "")) "with a nil")
  (is (string= nil (prefixp '() nil)) "with void list"))

(test suffixp
  (is (string= "bar" (suffixp '("foobar" "teambar") "bar")) "default case")
  (is (string= "r" (suffixp '("foobar" "teambar") "r")) "smaller suffix")
  (is (string= nil (suffixp'("foobar" "barfoo") "x")) "not a common suffix")
  (is (string= "" (suffixp'("foobar" "barfoo") "")) "suffix is a a void string")
  (is (string= "" (suffixp '("foobar" "") "")) "with empty string")
  (is (string= nil (suffixp '("foobar" nil) "")) "with a nil")
  (is (string= nil (suffixp '() nil)) "with void list"))

(test wrapped-in-p
  (is (string= "/foo/" (wrapped-in-p "/" "/foo/")) "default case")
  (is (string= nil (wrapped-in-p "/" "/foo")) "false case")
  (is (string= "/foo/" (wrapped-in-p "" "/foo/")) "blank start/end")
  (is (string= "/foo/" (wrapped-in-p nil "/foo/")) "blank start/end")
  (is (string= nil (wrapped-in-p nil nil)) "nils")
  (is (string= "" (wrapped-in-p nil "")) "nil and blank")
  (is (string= nil (wrapped-in-p "" nil)) "blank and nil")
  (is (string= nil (wrapped-in-p #\/ "/foo")) "with a char")
  (is (string= "<3lisp<3" (wrapped-in-p "<3" "<3lisp<3")) "with a longer prefix."))

(test downcasep
  (is (not (downcasep nil)) "downcasep nil")
  (is (not (downcasep "")) "downcasep empty string")
  (is (not (downcasep " ")) "downcasep blank string")
  (is (downcasep " e ") "downcasep ok with spaces")
  (is (downcasep "rst") "downcasep default")
  (is (not (downcasep " A ")) "downcasep false default")
  (is (not (downcasep " aiue tsuie+- Aa ")) "downcasep false default")
  (is (not (downcasep " +,. ")) "downcasep only punctuation")
  (is (not (downcasep "123")) "downcasep only digits")
  (is (downcasep " a+,. ") "downcasep with punctuation (python api)")
  (is (downcasep " +,é. ") "downcasep with accent"))

(test upcasep
  (is (not (upcasep nil)) "upcasep nil")
  (is (not (upcasep "")) "upcasep empty")
  (is (not (upcasep "   ")) "upcasep blank")
  (is (not (upcasep "rst")) "upcasep lowercase letters")
  (is (not (upcasep "??+,")) "upcasep only punctuation")
  (is (not (upcasep "123")) "upcasep only digits")
  (is (upcasep " ++YES-- ") "upcasep default")
  (is (upcasep " ++YÉÈS-- ") "upcasep with accent")
  (is (not (upcasep " ++NO ñ-- ")) "upcasep with lower non-alpha ñ")
  (is (upcasep " ++YES Ñ-- ") "upcasep with non-alpha Ñ")
  (is (upcasep " ++Ñ-- ") "upcasep with only non-alpha"))

(test alphanump
  (is (alphanump "rst124ldv") "alphanump default")
  (is (not (alphanump " rst123ldv ")) "alphanump no space")
  (is (not (alphanump "rst,123+ldv")) "alphanump no punctuation")
  (is (not (alphanump ",+")) "alphanump no punctuation")
  (is (not (alphanump "abcéß
")) "not alphanump with newline"))

(test alphap
  (is (alphap "abcDEf") "alphap default")
  (is (not (alphap "abc,de")) "alphap no punctuation")
  (is (not (alphap "abcdeé")) "alphap no accents")
  (is (not (alphap "abc de")) "alphap no space")
  (is (not (alphap " ")) "alphap blank")
  (is (not (alphap "abc
")) "not alphap with newline"))

(test digitp
  (is (not (digitp "abc")) "digitp letters")
  (is (digitp "123") "digitp default")
  (is (not (digitp "123,456")) "digitp no punctuation")
  (is (not (digitp "123 456")) "digitp no space"))

(test has-alpha-p
  (is (has-alpha-p "-+,A-") "has-alpha-p default")
  (is (not (has-alpha-p "-é-")) "has-alpha-p no accents"))

(test has-alphanum-p
  (is (has-alphanum-p "-+, 1 ") "has-alphanum-p with digit"))

(def-suite case-functions
  :in str
  :description "Functions that change the case.")
(in-suite case-functions)

(test downcase
  (is (string= nil (downcase nil)) "downcase nil returns nil, not a string.")
  (is (string= "foo" (downcase "Foo")))
  (is (string= "foo" (downcase :foo)) "Built-in functions also work on symbols."))

(test upcase
  (is (string= nil (upcase nil)) "upcase nil returns nil, not a string.")
  (is (string= "FOO" (upcase "foo"))))

(test capitalize
  (is (string= nil (capitalize nil)) "capitalize nil returns nil, not a string.")
  (is (string= "Foo" (capitalize "foo"))))

(test no-case
  (is (string= nil (no-case nil)) "No-case returns nil, not a string.")
  (is (string= "foo" (no-case "Foo")))
  (is (string= "foo" (no-case :foo)) "Works also on symbols."))

(test camel-case
  (is (string= nil (camel-case nil)) "Camel-case returns nil, not a string.")
  (is (string= "fooFoo" (camel-case "Foo Foo")))
  (is (string= "fooFoo" (camel-case :foo.foo)) "Works also on symbols."))

(test dot-case
  (is (string= nil (dot-case nil)) "Dot-case returns nil, not a string.")
  (is (string= "foo.foo" (dot-case "Foo Foo")))
  (is (string= "foo.foo" (dot-case :foo-foo)) "Works also on symbols."))

(test header-case
  (is (string= nil (header-case nil)) "Header-case returns nil, not a string.")
  (is (string= "Foo-Foo" (header-case "Foo Foo")))
  (is (string= "Foo-Foo" (header-case :foo-foo)) "Works also on symbols."))

(test param-case
  (is (string= nil (param-case nil)) "Param-case returns nil, not a string.")
  (is (string= "foo-foo" (param-case "Foo Foo")))
  (is (string= "foo-foo" (param-case :foo.foo)) "Works also on symbols."))

(test pascal-case
  (is (string= nil (pascal-case nil)) "pascal-case returns nil, not a string.")
  (is (string= "FooFoo" (pascal-case "Foo Foo")))
  (is (string= "FooFoo" (pascal-case :foo.foo)) "Works also on symbols."))

(test path-case
  (is (string= nil (path-case nil)) "Path-case returns nil, not a string.")
  (is (string= "foo/foo" (path-case "Foo Foo")))
  (is (string= "foo/foo" (path-case :foo.foo)) "Works also on symbols."))

(test sentence-case
  (is (string= nil (sentence-case nil)) "sentence-case returns nil, not a string.")
  (is (string= "Foo foo" (sentence-case "Foo Foo")))
  (is (string= "Foo foo" (sentence-case :foo.foo)) "Works also on symbols."))

(test snake-case
  (is (string= nil (snake-case nil)) "snake-case returns nil, not a string.")
  (is (string= "foo_foo" (snake-case "Foo Foo")))
  (is (string= "foo_foo" (snake-case :foo.foo)) "Works also on symbols."))

(test swap-case
  (is (string= nil (swap-case nil)) "swap-case returns nil, not a string.")
  (is (string= "fOO fOO" (swap-case "Foo Foo")))
  (is (string= "foo.foo" (swap-case :FOO.FOO)) "Works also on symbols."))

(test title-case
  (is (string= nil (title-case nil)) "swap-case returns nil, not a string.")
  (is (string= "Fo O Foo" (title-case "FoO foo")))
  (is (string= "Foo Foo" (title-case :foo.foo)) "Works also on symbols."))

(test constant-case
  (is (string= nil (constant-case nil)) "constant-case returns nil, not a string.")
  (is (string= "FOO_FOO" (constant-case "Foo Foo")))
  (is (string= "FOO_FOO" (constant-case :foo.foo)) "Works also on symbols."))

(def-suite miscellaneous
  :in str)
(in-suite miscellaneous)

(test repeat
  (is (string= "" (repeat 10 "")))
  (is (string= "" (repeat 10 nil)))
  (is (string= "foofoofoo" (repeat 3 "foo")))
  (is (= (length (repeat 100000 "c")) 100000)))

(test fit
  (is (string= "hello" (fit 5 "hello"))
      "fit - base case, do nothing")
  (is (string= "hello+++++" (fit 10 "hello" :pad-char "+"))
      "fit -> pad")
  (is (string= "he…" (fit 3 "hello" :ellipsis "…"))
      "fit -> shorten"))

(test s-assoc-value
  (let ((alist '(("test" . 1) ("another test" . 2))))
    (is (equalp (values 1 (first alist)) (s-assoc-value alist "test")))
    (is (equalp (values 2 (second alist)) (s-assoc-value alist "another test")))
    (is (equalp (values nil nil) (s-assoc-value alist "a third test")))))

(test count-substring
  (is (not (count-substring nil nil)))
  (is (not (count-substring "" "abc")))
  (is (= 1 (count-substring "aba" "ababab")))
  (is (= 2 (count-substring "aba" "abababa")))
  (is (= 3 (count-substring "ab" "abxabxab")))
  (is (= 0 (count-substring "cd" "abxabxab")))
  (is (= 1 (count-substring "abcd" "abcd")))
  (is (= 0 (count-substring "abcde" "abcd")))
  (is (= 1 (count-substring "ab" "abxabxab" :start 3 :end 7)))
  (is (= 1 (count-substring "a" "abA")))
  (is (= 2 (count-substring "a" "abA" :ignore-case t))))

(test string-case
  (is (string-case "hello"
        ("hello" (format nil "yes"))
        (otherwise nil))
      "string-case base case")
  (is (string-case "two-forms"
	("first" nil)
	(("hello" "two-forms") t)
	(otherwise nil))
      "multiple-item clause")
  (is (string= nil (string-case "no"
                     ("hello" t)
                     (otherwise nil)))
      "otherwise form")
  (is (string= :otherwise (string-case :no-str
                            ("hello" t)
                            (otherwise :otherwise)))
      "matching with a symbol: otherwise"))

(test match
  (is (= 3
         (match "a 1 2 b"
           (("a " x " " y " b") (+ (parse-integer x)
                                   (parse-integer y)))
           (t 4)))
      "match the numbers inside")
  (is (string= "HELLO WORLD"
               (match "33hello world44"
                 (("33" x "44") (str:upcase x))
                 (t "no hello")))
      "match string inside")
  (is (string= "Lisp = fun"
               (match "33Lisp is fun44"
                 (("3" x "5") x)
                 (("3" "3" lisp " is " fun "4" "4")
                  (str:concat lisp " = " fun))
                 (t "no fun")))
      "match multi parts")
  (is (= 13
         (match "33Lisp is fun44"
           (("3" x "4") (length x))
           (("3" "3" x "4" "4")
            (length x))
           (t 0)))
      "match the first match")
  (is (eq 'gmail
          (match "lisp@gmail.com"
            ((_ "gmail" _) 'gmail)
            ((_ "outlook" _) 'outlook)
            ((_ "yahoo" _) 'yahoo)))
      "match with the placeholders")
  (is (string= " hello "
               (match "123 hello 456"
                 (("\\d+" s "\\d+")
                  s)
                 (t "nothing")))
      "match with the regex")
  (is (string= " hello "
               (match "123 hello 456"
                 (("\\d+" s "\\d*")
                  s)
                 (t "nothing")))
      "match with the regex"))
