#
# A simple demonstration of the closure construct.
# Copyright 1992, 2020-2022 Krister Joas.
#
# How to use:
#   (setq acct (make-account 1000)) -> #<closure 7fe0a8024c20>
#   ((acct 'withdraw) 100) -> 900
#   ((acct 'deposit) 200) -> 1100
#
(defineq
  (make-account
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
          '(balance withdraw deposit))))))
