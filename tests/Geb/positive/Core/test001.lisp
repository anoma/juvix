(defpackage #:test001
  (:shadowing-import-from :geb.lambda.spec #:func #:pair)
  (:shadowing-import-from :geb.spec #:case)
  (:use #:common-lisp #:geb.lambda.spec #:geb))

(in-package :test001)

(defparameter *entry*
  (typed
    (app
      (coprod
        so1
        so1)
      (coprod
        so1
        so1)
      (lamb
        (coprod
          so1
          so1)
        (coprod
          so1
          so1)
        (case-on
          so1
          so1
          (coprod
            so1
            so1)
          (index 0)
          (lamb
            so1
            (coprod
              so1
              so1)
            (right
              unit))
          (lamb
            so1
            (coprod
              so1
              so1)
            (left
              unit))))
      (left
        unit))
    (coprod
      so1
      so1)))
