#
# A shot at a foreach lisp shell function/command.
#
# $Id$
#

(df foreach (v x . f)
    (mapc (expand x t)
          (cons 'lambda
                (cons (cons v) f))))
