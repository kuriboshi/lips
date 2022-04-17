#
# A few miscellaneous utility functions.
#
# $Id$
#
(load "lisp/let")

(defineq
  ;; Reverse a list.
  (reverse
   (lambda (l r)
     (cond
       ((null l) r)
       (t (reverse (cdr l) (cons (car l) r))))))

  ;; Append implemented in lisp.
  (append2
   (lambda (x y)
     (cond
       ((null x) y)
       (t (cons (car x)
                (append2 (cdr x) y))))))

  ;; Intersection of two lists.
  ;; (intersection '(0 1 2 3) '(2 3 4 5)) => (2 3)
  (intersection
   (lambda (x y)
     (cond
       ((null x) nil)
       ((memb (car x) y)
        (cons (car x) (intersection (cdr x) y)))
       (t (intersection (cdr x) y)))))

  ;; Substitute any y in l with x.
  (subst
   (lambda (x y l)
     (cond
       ((null l) nil)
       ((equal y (car l))
        (cons x (subst x y (cdr l))))
       (t (cons (car l) (subst x y (cdr l)))))))

  ;; Substitute elements in l according to the assoc list in al.
  ;; (sublis '((0 . a) (1 . b) (2 . c)) '(0 1 2)) => (a b c)
  (sublis
   (lambda (al l)
     (cond
       ((nlistp l)
        (cond
          ((setq temp (assoc l al))
           (cdr temp))
          (t l)))
       (t (cons (sublis al (car l))
                (sublis al (cdr l)))))))

  ;; Implements association lists.
  ;; (assoc 'b '((a . 0) (b . 1) (c . 2))) => (b . 1)
  (assoc
   (lambda (x al)
     (let (y)
       (while al
         (cond
           ((eq x (caar al))
            (setq y (car al))
            (setq al))
           (t (setq al (cdr al)))))
       y)))

  ;; Flattens a list.
  ;; (flatten '(a (b (c)))) => (a b c)
  (flatten
   (lambda (l)
     (cond
       ((nlistp l) l)
       ((atom (car l))
        (cons (car l) (flatten (cdr l))))
       (t (append2 (flatten (car l)) (flatten (cdr l)))))))
  )
