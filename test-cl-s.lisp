(in-package :cl-user)
(defpackage test-cl-s
  (:use :cl
        :prove
        ;;
        :cl-s
        ))

(in-package :cl-s)

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
  (is "foo" (replace "a" "o" "faa"))
  (is "foo" (replace "^a" "o" "fo^a"))
  (is "foo" (replace "^aa+" "o" "fo^aa+"))
  )

(subtest "Join"
  (is "foo bar baz" (join " " '("foo" "bar" "baz")))
  (is "foo+++bar+++baz" (join "+++" '("foo" "bar" "baz")))
  (is "foo~bar" (join "~" '("foo" "bar")))
  (is "foo~~~bar" (join "~" '("foo~" "~bar")))
  )

(subtest "Split"
  (is '("foo" "bar") (split " " "foo bar"))
  (is '("foo" "bar") (split "\\+" "foo+bar"))
  )

(subtest "Blank-p"
  (ok (blank? nil))
  (ok (blank? ""))
  (is nil (blank? " "))
  )

(subtest "Blank string"
  (ok (blank-str-p "  "))
  )

;; prove end
(finalize)
