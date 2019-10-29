(in-package #:asdf-user)

(defsystem :str
  :author "vindarel <vindarel@mailz.org>"
  :maintainer "vindarel <vindarel@mailz.org>"
  :license "MIT"
  :version "0.15"
  :homepage "https://github.com/vindarel/cl-str"
  :bug-tracker "https://github.com/vindarel/cl-str/issues"
  :source-control (:git "git@github.com:vindarel/cl-str.git")
  :description "Modern, consistent and terse Common Lisp string manipulation library."
  :depends-on (:cl-ppcre
               :cl-ppcre-unicode
               :cl-change-case)
  :components ((:file "str"))

  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op :str.test))))
