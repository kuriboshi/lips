#
# alias -- functions defining the alias commands
# Copyright 1989, 2020 Krister Joas.
#
# $Id$
#
(setq aliaslist)
(df alias (a . b)
    (cond ((and (null a) (null b))
           (mapc aliaslist
                 '(lambda (x)
                    (prin2 x)
                    (prin1 "	")
                    (print (getprop x 'alias)))))
          ((null b)
           (print (getprop a 'alias)))
          (t (putprop a 'alias b)
             (cond
              ((memb a aliaslist))
              (t (setq aliaslist (cons a aliaslist)))))))
(df unalias (a)
    (cond ((null a))
          (t (remprop a 'alias))))
