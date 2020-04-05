#
# A simple demonstration of the closure construct.
# Copyright 1992, Krister Joas.
#
# $Id$
#
(setq make-account
 (lambda (balance)
  ((closure
    '(progn
      (setq withdraw
            (closure
             (lambda (amount)
              (setq balance (difference balance amount)))
             '(balance)))
      (setq deposit
            (closure
             (lambda (amount)
              (setq balance (plus balance amount)))
             '(balance)))
      (lambda ()
       (closure
        (lambda (m)
         (cond
          ((eq m 'withdraw) withdraw)
          ((eq m 'deposit) deposit)
          (t nil)))
       '(withdraw deposit))))
    '(balance withdraw deposit)))))
