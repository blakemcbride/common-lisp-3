
(defpackage :cl3
  (:use "COMMON-LISP")
  (:export "DEFINE" "DEFINE-MACRO" "DEFINE-METHOD" "LISP1"      "SET-SLOT" "GET-SLOT" "metaclass" "DEFINE-CLASS" "DEFINE-METHOD")
  (:shadow "LOAD" "COMPILE-FILE" "EVAL" "DEFINE" "DEFINE-METHOD"))
(in-package :cl3)
(provide :cl3)

(cl:load "clos-utils")
(cl:load "lisp1")

; make lisp case sensitive
(setf (readtable-case *readtable*) :invert)
