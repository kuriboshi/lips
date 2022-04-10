#
# let -- the let function
# Copyright 1992, 2020 Krister Joas.
#
# $Id$
#
(defineq
  (let (nlambda (init . seq)
                (eval (cons (cons 'lambda
                                  (cons (vlis init) seq))
                            (alis init)))))

  (vlis (lambda (u)
          (cond
           ((null u) nil)
           ((atom (car u)) (cons (car u) (vlis (cdr u))))
           (t (cons (caar u) (vlis (cdr u)))))))

  (alis (lambda (u)
          (cond
           ((null u) nil)
           ((atom (car u)) (cons nil (alis (cdr u))))
           (t (cons (car (cdar u))
                    (alis (cdr u))))))))
