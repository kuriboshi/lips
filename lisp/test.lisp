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
  ;; Define a test with the name NAME and a description DESC. The
  ;; expressions in the BODY is evaluated in sequence.
  (deftest
    (nlambda (name desc . body)
             (tconc *tests* (list name desc body))))

  ;; Define a section within a test with the description DESC. The
  ;; expressions in BODY are evaluated in sequence.
  (section
   (nlambda (desc . body)
            (mapc body eval)))

  ;; Compare the result to the expected result using the function
  ;; FN. Record the result in *testresult*. Each result is a list of
  ;; five elements: the name of the test, the current check number
  ;; within a test, the symbol passed or failed, the result, and the
  ;; expected result.
  (check
   (lambda (fn result expected)
     (setq *numcheck* (add1 *numcheck*))
     (tconc *testresult*
            (list *currenttest* *numcheck*
                  (cond ((fn result expected) 'passed)
                        (t 'failed))
                  result expected))))

  ;; Runs the tests.
  (runtest
   (lambda (which)
     (mapc (car *tests*)
           (lambda (test)
             (let ((*numcheck* 0))
               (cond ((or (null which)
                          (eq which (car test)))
                      (if *verbose*
                          (progn
                            (prin1 (list "Running" (car test)))
                            (terpri)))
                      (let ((*currenttest* (car test)))
                        (mapc (caddr test) eval)))))))
     (car *testresult*)))

  ;; Print each element in the list L using prin1 followed by a
  ;; newline using terpri.
  (printlist1
   (lambda l
     (mapc l prin1)
     (terpri)))

  ;; Print the result of each test (if *verbose* is t) and collect the
  ;; number of passed and failed tests for the summary.
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

  ;; Print a summary of the test run.
  (printsummary
   (lambda ()
     (printlist1 "Passed: " passed)
     (printlist1 "Failed: " failed)))

  ;; Prints a report of the test result.
  (reporttest
   (lambda ()
     (let ((passed 0)
           (failed 0))
       (mapc (car *testresult*) printresult)
       (printsummary))))
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

(deftest test-apply "Test apply"
  (section "(apply car '((1 2 3)))"
           (check eq
                  (apply car '((1 2 3)))
                  1))
  (section "(apply* car '(1 2 3))"
           (check eq
                  (apply* car '(1 2 3))
                  1)))

(runtest)
(reporttest)
