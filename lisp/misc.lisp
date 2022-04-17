#
# A shot at a foreach lisp shell function/command.
#
# $Id$
#

(defineq
  (foreach
   (nlambda (v x . f)
     (mapc (expand x t)
           (cons 'lambda
                 (cons (cons v) f)))))
    )
