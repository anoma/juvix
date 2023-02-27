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
              so1
              so1
              unit))
          (lamb
            so1
            (coprod
              so1
              so1)
            (left
              so1
              so1
              unit))))
      (left
        so1
        so1
        unit))
    (coprod
      so1
      so1)))
