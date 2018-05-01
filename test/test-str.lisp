(in-package :cl-user)
(defpackage test-str
  (:use :cl
        :prove
        :str
        ))

(in-package :test-str)

(setf prove:*enable-colors* t)
(plan nil)

(subtest "Trim"
  (is "rst " (trim-left "   rst "))
  (is " rst" (trim-right " rst   "))
  (is "rst" (trim "  rst  "))
  )

(subtest "Concat"
  (is "foo" (concat "f" "o" "o"))
  (is "" (concat))
  )

(subtest "Replace"
  (is "foo" (replace-all "a" "o" "faa"))
  (is "foo" (replace-all "^a" "o" "fo^a"))
  (is "foo" (replace-all "^aa+" "o" "fo^aa+"))
  )

(subtest "Join"
  (is "foo bar baz" (join " " '("foo" "bar" "baz")))
  (is "foo+++bar+++baz" (join "+++" '("foo" "bar" "baz")))
  (is "foo~bar" (join "~" '("foo" "bar")))
  (is "foo~~~bar" (join "~" '("foo~" "~bar")))
  )

(subtest "Split"
  (is '("foo" "bar") (split " " "foo bar"))
  (is '("foo" "bar") (split "+" "foo+bar") "separator is a regexp")
  (is '("foo" "bar" "") (split "+" "foo+bar+") "a trailing separator returns a void string, unlike in cl-ppcre")
  (is '("foo" "" "bar") (split "+" "foo++bar"))
  (is '("foo" "bar") (split "+" "foo+bar"))
  (is '("foo" "bar") (split "x" "fooxbar") "split with string x")
  (is '("foo" "bar") (split #\x "fooxbar") "split with character")
  (is '("foo" "bar") (split "(*)" "foo(*)bar"))
  (is '("foo" "bar" "") (split "NO" "fooNObarNO") "separator is a string")
  (is '("foo" "bar") (split "+" "foo+++bar++++" :omit-nulls t) "omit-nulls argument")
  (is '("foo" "   ") (split "+" "foo+++   ++++" :omit-nulls t) "omit-nulls and blanks")
  )

(subtest "substring"
  (is "abcd" (substring 0 4 "abcd") "normal case")
  (is "ab" (substring 0 2 "abcd") "normal case substing")
  (is "bc" (substring 1 3 "abcd") "normal case substing middle")
  (is "" (substring 4 4 "abcd") "normal case")
  (is "" (substring 0 0 "abcd") "normal case")
  (is "d" (substring 3 4 "abcd") "normal case")
  (is "abcd" (substring 0 t "abcd") "end is t")
  (is "abcd" (substring 0 nil "abcd") "end is nil")
  (is "abcd" (substring 0 100 "abcd") "end is too large")
  (is "abc" (substring 0 -1 "abcd") "end is negative")
  (is "b" (substring 1 -2 "abcd") "end is negative")
  (is "" (substring 2 1 "abcd") "start is bigger than end")
  (is "" (substring 0 -100 "abcd") "end is too low")
  (is "" (substring 100 1 "abcd") "start is too big")
  (is "abcd" (substring -100 4 "abcd") "start is too low")
  (is "abcd" (substring -100 100 "abcd") "start and end are too low and big")
  (is "" (substring 100 -100 "abcd") "start and end are too big and low")
  )

(subtest "Repeat"
  (is "" (repeat 10 ""))
  (is "foofoofoo" (repeat 3 "foo")))

(subtest "Empty-p"
  (ok (empty? nil))
  (ok (emptyp ""))
  (is nil (empty? " "))
  )

(subtest "Blank string"
  (ok (blankp "  "))
  (ok (blank? "  "))
  (is nil (blank? "   rst "))
  )

(subtest "Words"
  (is nil (words nil))
  (is nil (words ""))
  (is '("foo") (words "foo"))
  (is '("foo" "bar") (words "foo bar"))
  (is '("foo" "bar") (words "  foo   bar   "))
  (is '("foo" "bar  ") (words " foo bar  " :limit 2))
  (is '("foo" "bar baz ") (words " foo bar baz " :limit 2))
  (is '("foo" "bar" "baz" "?") (words "  foo bar  baz ?"))
  (is '("foo" "bar" "baz" "?") (words "  foo
 bar  baz ?"))
  )

(subtest "Unwords"
  (is "" (unwords nil))
  (is "" (unwords '()))
  (is "" (unwords '("")))
  (is "foo" (unwords '("foo")))
  (is "foo bar baz" (unwords '("foo bar baz"))))


(subtest "Lines"
  (is '("") (lines nil))
  (is '("") (lines ""))
  (is '("" "") (lines "
"))
  (is '("1" "2" " 3") (lines "1
2
 3"))
  (is '("1" "2" " 3" "") (lines "1
2
 3
")))

(subtest "Unlines"
  (is "" (unlines nil))
  (is "" (unlines '("")))
  (is "
" (unlines '("" "")))
  (is "1
2
" (unlines '("1" "2" ""))))

(subtest "starts-with?"
  (ok (starts-with? "foo" "foobar") "default case")
  (ok (starts-with? "" "foo") "with blank start")
  (ok (not (starts-with? "rs" "")) "with blank s")
  (ok (not (starts-with? "foobar" "foo")) "with shorter s")
  (ok (starts-with? "" "") "with everything blank")
  (ok (not (starts-with? "FOO" "foobar")) "don't ignore case")
  (ok (starts-with-p "f" "foo") "starts-with-p alias")
  (ok (starts-with? "FOO" "foobar" :ignore-case t) "ignore case"))

(subtest "ends-with?"
  (ok (ends-with? "bar" "foobar") "default case")
  (ok (ends-with-p "bar" "foobar") "ends-with-p alias")
  (ok (not (ends-with? "BAR" "foobar")) "don't ignore case")
  (ok (ends-with? "BAR" "foobar" :ignore-case t) "ignore case"))

(subtest "prefix"
  (is (prefix '("foobar" "footeam")) "foo" "default case")
  (is (prefix '("foobar" "barfoo")) "" "no common prefix")
  (is (prefix '("foobar" "")) "" "with empty string")
  (is (prefix '("foobar" nil)) "" "with a nil")
  (is (prefix '()) nil "with void list"))

(subtest "prefix?"
  (is (prefix? '("foobar" "footeam") "foo") "foo" "default case")
  (is (prefix?'("foobar" "barfoo") "") "" "no common prefix")
  (is (prefix? '("foobar" "") "") "" "with empty string")
  (is (prefix? '("foobar" nil) "") "" "with a nil")
  (is (prefix? '() nil) nil "with void list"))

(subtest "suffix"
  (is (suffix '("foobar" "teambar")) "bar" "default case")
  (is (suffix '("foobar" "barfoo")) "" "no common suffix")
  (is (suffix '("foobar" "")) "" "with empty string")
  (is (suffix '("foobar" nil)) "" "with a nil")
  (is (suffix '()) nil "with void list"))

(subtest "suffix?"
  (is (suffix? '("foobar" "teambar") "bar") "bar" "default case")
  (is (suffix?'("foobar" "barfoo") "") "" "no common suffix")
  (is (suffix? '("foobar" "") "") "" "with empty string")
  (is (suffix? '("foobar" nil) "") "" "with a nil")
  (is (suffix? '() nil) nil "with void list"))

(subtest "contains?"
  (ok (contains? "foo" "blafoobar") "default")
  (ok (not (contains? "foo" "")) "with no string")
  (ok (not (contains? "" nil)) "a blank substring in a nil str")
  (ok (not (contains? "foo" nil)) "with string nil")
  (ok (not (contains? "Foo" "blafoobar")) "with case")
  (ok (contains? "Foo" "blafoobar" :ignore-case t) "ignore case")
  (ok (containsp "Foo" "blafoobar" :ignore-case t) "containsp alias")
  )

(subtest "string-case"
  (ok (string-case "hello"
        ("hello" (format nil "yes"))
        (otherwise nil))
      "string-case base case")
  (ok (not (string-case "no"
             ("hello" t)
             (otherwise nil)))
      "otherwise form")
  (is :otherwise (string-case :no-str
                   ("hello" t)
                   (otherwise :otherwise))
      "matching with a symbol: otherwise"))

(subtest "s-first"
  (is (string-car "foobar") "f")
  (is (string-car "") ""))

(subtest "s-rest"
  (is (string-cdr "foobar") "oobar")
  (is (string-cdr "") ""))

(subtest "s-nth"
  (is (string-nth 3 "foobar") "b")
  (is (string-nth -1 "foobar") "")
  (is (string-nth 6 "foobar") "")
  (is (string-nth 3 "") ""))

;; prove end
(finalize)
