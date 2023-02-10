(defpackage #:test004
  (:shadowing-import-from :geb.lambda.spec #:func #:pair)
  (:shadowing-import-from :geb.spec #:case)
  (:use #:common-lisp #:geb.lambda.spec #:geb))

(in-package :test004)

(defparameter *entry*
  (typed
    (app
      (!->
        (coprod so1 so1) ;; domain of the function
        (coprod so1 so1) ;; codomain of the function
        )
      (coprod
        so1
        so1)
      (lamb
        (!->
          (coprod
            so1
            so1)
          (coprod
            so1
            so1))
        (coprod
          so1
          so1)
        (app
          (!->
            (coprod
              so1
              so1)
            (!->
              (coprod
                so1
                so1)
              (coprod
                so1
                so1)))
          (coprod
            so1
            so1)
          (lamb
            (!->
              (coprod
                so1
                so1)
              (!->
                (coprod
                  so1
                  so1)
                (coprod
                  so1
                  so1)))
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
              (app
                (coprod
                  so1
                  so1)
                (!->
                  (coprod
                    so1
                    so1)
                  (coprod
                    so1
                    so1))
                (index 0)
                (app
                  (coprod
                    so1
                    so1)
                  (coprod
                    so1
                    so1)
                  (index 1)
                  (right
                    unit)))
              (app
                (coprod
                  so1
                  so1)
                (coprod
                  so1
                  so1)
                (index 1)
                (app
                  (coprod
                    so1
                    so1)
                  (coprod
                    so1
                    so1)
                  (index 1)
                  (left
                    unit)))))
          (lamb
            (coprod
              so1
              so1)
            (!->
              (coprod
                so1
                so1)
              (coprod
                so1
                so1))
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
                (index 1)
                (lamb
                  so1
                  (coprod
                    so1
                    so1)
                  (index 1))
                (lamb
                  so1
                  (coprod
                    so1
                    so1)
                  (right
                    unit)))))))
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
              unit)))))
    (coprod
      so1
      so1)))
