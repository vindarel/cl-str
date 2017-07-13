(in-package :cl-user)
(defpackage test-str
  (:use :cl
        :prove
        ;;
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

(subtest "Starts-with"
  (ok (starts-with? "foo" "foobar") "default case")
  (ok (starts-with? "" "foo") "with blank start")
  (ok (not (starts-with? "rs" "")) "with blank s")
  (ok (not (starts-with? "foobar" "foo")) "with shorter s")
  (ok (starts-with? "" "") "with everything blank")
  (ok (not (starts-with? "FOO" "foobar")) "don't ignore case")
  (ok (starts-with-p "f" "foo") "starts-with-p alias")
  (ok (starts-with? "FOO" "foobar" :ignore-case t) "ignore case"))

(subtest "ends-with"
  (ok (ends-with? "bar" "foobar") "default case")
  (ok (ends-with-p "bar" "foobar") "ends-with-p alias")
  (ok (not (ends-with? "BAR" "foobar")) "don't ignore case")
  (ok (ends-with? "BAR" "foobar" :ignore-case t) "ignore case"))

;; prove end
(finalize)
