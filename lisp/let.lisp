#
# let -- the let function
# Copyright 1992, Krister Joas.
#
# $Id$
#
(df let (init . seq)
   (eval (cons (cons 'lambda
                     (cons (vlis init) seq))
               (alis init))))

(de vlis (u)
 (cond
  ((null u) nil)
  ((atom (car u)) (cons (car u) (vlis (cdr u))))
  (t (cons (caar u) (vlis (cdr u))))))

(de alis (u)
 (cond
  ((null u) nil)
  ((atom (car u)) (cons nil (alis (cdr u))))
  (t (cons (car (cdar u))
           (alis (cdr u))))))
