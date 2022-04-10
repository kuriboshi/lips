#
# Lips -- lisp shell
# Copyright 2022 Krister Joas <krister@joas.jp>
#

#
# Super simple unit test suite.
#

(load "lisp/let")

;;; List of tests. The list of tests is built using TCONC so to get
;;; the actual list of tests call (car *tests*).
(setq *tests* (cons))
;;; Collects the result of the tests.
(setq *testresult* (cons))
;;; If T print result of each individual test.
(setq *verbose* nil)

(defineq
  (deftest
    (nlambda (name desc . body)
             (tconc *tests* (list name desc body))))

  ;; Runs the tests.
  (runtest
   (lambda (which)
     (let ((*numcheck* 0))
       (mapc (car *tests*)
             (lambda (test)
               (cond ((or (null which)
                          (eq which (car test)))
                      (if *verbose*
                          (progn
                            (prin1 (list "Running" (car test)))
                            (terpri)))
                      (let ((*currenttest* (car test)))
                        (mapc (caddr test) null)))))))
     (car *testresult*)))

  (printlist1
   (lambda l
     (mapc l (lambda (a) (prin1 a)))
     (terpri)))
;     (print l)
;     (cond ((null l) (terpri))
;           (t (printlist1 (cdr l))))))
;           (t (prin1 (car l))
;              (printlist1 (cdr l))))))

  (printresult
   (lambda (result)
     (cond ((eq (caddr result) 'passed)
            (if *verbose*
                (printlist1 (car result) " " (cadr result) " passed")))
           (t (printlist1 (car result) " " (cadr result) " failed")
              (printlist1 "  result:   " (nth result 4))
              (printlist1 "  expected: " (nth result 5))))
     (cond ((eq (caddr result) 'passed)
            (setq passed (add1 passed)))
           (t (setq failed (add1 failed))))))

  (printsummary
   (lambda ()
     (prin1 "Passed: ")
     (prin1 passed)
     (terpri)
     (prin1 "Failed: ")
     (prin1 failed)
     (terpri)))

  ;; Prints a report of the tests
  (reporttest
   (lambda ()
     ((lambda (passed failed)
        (mapc (car *testresult*) printresult)
        (printsummary))
      0 0)))

  (section
   (nlambda (desc . p)
     (mapc p null)))

  (check
   (lambda (fn result expected)
     (setq *numcheck* (add1 *numcheck*))
     (tconc *testresult*
            (list *currenttest* *numcheck*
                  (cond ((fn result expected) 'passed)
                        (t 'failed))
                  result expected))))
  )

(deftest test-null "Test null"
  (section "(null nil) == t"
           (check eq (null nil) t))
  (section "(null t) == nil"
           (check eq (null t) nil))
  (section "(null 100) == nil"
           (check eq (null 100) nil)))

(deftest test-mapcar "Test mapcar"
  (section "(mapcar '(1 2 3) add1)"
           (check equal
                  (mapcar '(1 2 3) add1)
                  '(2 3 4))))

(runtest)
(reporttest)
