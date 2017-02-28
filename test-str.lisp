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
  (is '("foo" "bar") (split "(*)" "foo(*)bar"))
  (is '("foo" "bar" "") (split "NO" "fooNObarNO") "separator is a string")
  )

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

;; prove end
(finalize)
