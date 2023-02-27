(defpackage #:test007
  (:shadowing-import-from :geb.lambda.spec #:func #:pair)
  (:shadowing-import-from :geb.spec #:case)
  (:use #:common-lisp #:geb.lambda.spec #:geb))

(in-package :test007)

(defparameter *entry*
  (typed
    (app
      (prod
        int
        int)
      int
      (lamb
        (prod
          int
          int)
        int
        (app
          (prod
            int
            int)
          int
          (lamb
            (prod
              int
              int)
            int
            (app
              int
              int
              (app
                int
                (!->
                  int
                  int)
                (lamb
                  int
                  (!->
                    int
                    int)
                  (lamb
                    int
                    int
                    (add
                      (index 1)
                      (index 0))))
                (fst
                  int
                  int
                  (index 0)))
              (snd
                int
                int
                (index 0))))
          (index 0)))
      (pair
        int
        int
        3
        5))
    int))
