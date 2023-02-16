(defpackage #:test006
  (:shadowing-import-from :geb.lambda.spec #:func #:pair)
  (:shadowing-import-from :geb.spec #:case)
  (:use #:common-lisp #:geb.lambda.spec #:geb))

(in-package :test006)

(defparameter *entry*
  (typed
    (app
      (!->
        int
        (!->
          int
          int))
      int
      (lamb
        (!->
          int
          (!->
            int
            int))
        int
        (app
          (!->
            int
            (!->
              int
              int))
          int
          (lamb
            (!->
              int
              (!->
                int
                int))
            int
            (app
              (!->
                (!->
                  int
                  (!->
                    int
                    int))
                (!->
                  int
                  (!->
                    int
                    int)))
              int
              (lamb
                (!->
                  (!->
                    int
                    (!->
                      int
                      int))
                  (!->
                    int
                    (!->
                      int
                      int)))
                int
                (app
                  int
                  int
                  (lamb
                    int
                    int
                    (app
                      int
                      int
                      (lamb
                        int
                        int
                        (app
                          (!->
                            int
                            int)
                          int
                          (lamb
                            (!->
                              int
                              int)
                            int
                            (app
                              int
                              int
                              (lamb
                                int
                                int
                                (app
                                  int
                                  int
                                  (lamb
                                    int
                                    int
                                    (app
                                      int
                                      int
                                      (lamb
                                        int
                                        int
                                        (add
                                          (add
                                            (add
                                              (app
                                                int
                                                int
                                                (index 3)
                                                (div
                                                  (index 4)
                                                  (index 5)))
                                              (add
                                                (mul
                                                  (index 2)
                                                  (index 5))
                                                (index 4)))
                                            (add
                                              (index 1)
                                              (mul
                                                (index 0)
                                                (add
                                                  (index 2)
                                                  1))))
                                          (app
                                            int
                                            int
                                            (app
                                              int
                                              (!->
                                                int
                                                int)
                                              (index 8)
                                              (app
                                                int
                                                int
                                                (app
                                                  int
                                                  (!->
                                                    int
                                                    int)
                                                  (app
                                                    (!->
                                                      int
                                                      (!->
                                                        int
                                                        int))
                                                    (!->
                                                      int
                                                      (!->
                                                        int
                                                        int))
                                                    (index 6)
                                                    (index 7))
                                                  2)
                                                3))
                                            4)))
                                      7))
                                  30))
                              0))
                          (lamb
                            int
                            int
                            (add
                              (index 0)
                              4))))
                      17))
                  5))
              (lamb
                (!->
                  int
                  (!->
                    int
                    int))
                (!->
                  int
                  (!->
                    int
                    int))
                (lamb
                  int
                  (!->
                    int
                    int)
                  (lamb
                    int
                    int
                    (mul
                      (app
                        int
                        int
                        (app
                          int
                          (!->
                            int
                            int)
                          (index 2)
                          (index 1))
                        (index 1))
                      (index 0)))))))
          (lamb
            int
            (!->
              int
              int)
            (lamb
              int
              int
              (sub
                (add
                  (index 1)
                  1)
                (mul
                  (index 0)
                  7))))))
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
            (index 0)))))
    int))
