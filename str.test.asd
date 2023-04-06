(in-package #:asdf-user)

(defsystem :str.test
  :author "vindarel <vindarel@mailz.org>"
  :maintainer "vindarel <vindarel@mailz.org>"
  :license "MIT"
  :description "Test suite for cl-str."
  :depends-on ("str" "fiveam")
  :pathname "test"
  :components ((:file "test-str"))
  :perform (asdf:test-op
            (o s)
            (uiop:symbol-call '#:fiveam '#:run!
                              (uiop:find-symbol* '#:str
                                                 '#:test-str))))
