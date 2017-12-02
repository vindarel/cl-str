(in-package #:asdf-user)

(defsystem :str.test
  :author "vindarel <ehvince@mailz.org>"
  :maintainer "vindarel <ehvince@mailz.org>"
  :license "MIT"
  :description "Test suite for cl-str."
  :depends-on (:str :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ()

  :perform (test-op (op system)
             (funcall (read-from-string "prove:run")
                      (system-relative-pathname :str.test "test/"))))
