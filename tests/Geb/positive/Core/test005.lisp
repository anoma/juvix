(defpackage #:test005
  (:shadowing-import-from :geb.lambda.spec #:func #:pair)
  (:shadowing-import-from :geb.spec #:case)
  (:use #:common-lisp #:geb.lambda.spec #:geb))

(in-package :test005)

(defparameter *entry*
  (typed
    (add
      5
      (mul
        2
        3))
    int))
