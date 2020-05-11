#
# A few miscellaneous utility functions.
#
# $Id$
#
(de reverse (l r)
  (cond
   ((null l) r)
   (t (reverse (cdr l) (cons (car l) r)))))

(de append2 (x y)
  (cond
   ((null x) y)
   (t (cons (car x)
            (append2 (cdr x) y)))))

(de intersection (x y)
  (cond
   ((null x) nil)
   ((memb (car x) y)
    (cons (car x) (intersection (cdr x) y)))
   (t (intersection (cdr x) y))))

(de subst (x y l)
  (cond
   ((null l) nil)
   ((equal y (car l))
    (cons x (subst x y (cdr l))))
   (t (cons (car l) (subst x y (cdr l))))))

(de sublis (al l)
  (cond
   ((nlistp l)
    (cond
     ((setq temp (assoc l al))
      (cdr temp))
     (t l)))
   (t (cons (sublis al (car l))
            (sublis al (cdr l))))))

(de assoc (x al)
  (let (y)
   (while al
     (cond
      ((eq x (caar al))
       (setq y (car al))
       (setq al))
      (t (setq al (cdr al)))))
   y))


(de flatten (l)
  (cond
   ((nlistp l) l)
   ((atom (car l))
    (cons (car l) (flatten (cdr l))))
   (t (append2 (flatten (car l)) (flatten (cdrl))))))
