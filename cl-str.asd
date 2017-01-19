(in-package :cl-user)
(defpackage str
  (:use :cl :asdf))
(in-package :str)

(defsystem str
  :author "vindarel <ehvince@mailz.org>"
  :maintainer "vindarel <ehvince@mailz.org>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/vindarel/cl-str"
  :bug-tracker ""
  :source-control (:git "git@github.com:vindarel/cl-s.git")
  :description "Modern, consistant and terse Common Lisp string manipulation library."
  :depends-on (:prove)
  :components ((:file "cl-str"))

  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-s-test))))
