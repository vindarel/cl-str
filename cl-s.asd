;; (in-package :cl-user)
;; (defpackage cl-s
  ;; (:use :cl :asdf))
;; (in-package :cl-s)

(defsystem cl-s
  :author "vindarel <ehvince@mailz.org>"
  :maintainer "vindarel <ehvince@mailz.org>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :description "The long lost Common Lisp string manipulation library."
  :depends-on (:prove)
  :components ((:file "cl-s"))

  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-s-test))))
