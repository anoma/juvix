(defpackage #:test002
  (:shadowing-import-from :geb.lambda.spec #:func #:pair)
  (:shadowing-import-from :geb.spec #:case)
  (:use #:common-lisp #:geb.lambda.spec #:geb))

(in-package :test002)

(defparameter *entry*
  (typed
    (app
      (coprod
        (coprod
          so1
          so1)
        so1)
      (coprod
        so1
        so1)
      (app
        (coprod
          so1
          so1)
        (!->
          (coprod
            (coprod
              so1
              so1)
            so1)
          (coprod
            so1
            so1))
        (lamb
          (coprod
            so1
            so1)
          (!->
            (coprod
              (coprod
                so1
                so1)
              so1)
            (coprod
              so1
              so1))
          (lamb
            (coprod
              (coprod
                so1
                so1)
              so1)
            (coprod
              so1
              so1)
            (case-on
              (coprod
                so1
                so1)
              so1
              (coprod
                so1
                so1)
              (index 0)
              (lamb
                (coprod
                  so1
                  so1)
                (coprod
                  so1
                  so1)
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
                      (index 3)
                      (lamb
                        so1
                        (coprod
                          so1
                          so1)
                        (left
                          unit))
                      (lamb
                        so1
                        (coprod
                          so1
                          so1)
                        (index 1))))
                  (index 1)))
              (lamb
                so1
                (coprod
                  so1
                  so1)
                (right
                  unit)))))
        (right
          unit))
      (left
        (left
          unit)))
    (coprod
      so1
      so1)))
