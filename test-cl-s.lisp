(in-package :cl-user)
(defpackage test-cl-s
  (:use :cl
        :prove
        ;;
        :cl-s
        ))

(in-package :test-cl-s)

(setf prove:*enable-colors* t)
(plan nil)

(subtest "Trim"
  (is "rst " (s-trim-left "   rst "))
  (is " rst" (s-trim-right " rst   "))
  (is "rst" (s-trim "  rst  "))
  )


(subtest "Join"
  (is "foo bar baz" (s-join " " '("foo" "bar" "baz")))
  (is "foo+++bar+++baz" (s-join "+++" '("foo" "bar" "baz")))
  )

(subtest "Split"
  (is '("foo" "bar") (s-split " " "foo bar"))
  (is '("foo" "bar") (s-split "\\+" "foo+bar"))
  )

;; prove end
(finalize)
