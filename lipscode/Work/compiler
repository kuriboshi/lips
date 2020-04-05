(defineq
(compexp (exp)
  (cond
   ((isconst exp)
    (list (mksend 1 exp)))
   (t ((lambda (z)
        (compapply (func exp)
                   (complis z)
                   (length z)))
       (arglist exp)))))

(complis (u)
  (cond
   ((null u) nil)
   ((null (cdr u))
    (compexp (car u)))
   (t (append2
       (append2 (compexp (car u))
                (list (mkalloc 1)))
       (complis (cdr u))))))

(compapply (fn vals n)
  (append2
   (append2 vals (mklink n))
   (list (mkcall fn))))

(isconst (x)
  (or (numberp x)
      (eq x 't)
      (null t)
      (and (null (atom x)) (eq (car x) 'quote))))

(func (x) (car x))
(arglist (x) (cdr x))

(mksend (dest val) (list 'movei dest val))
(mkalloc (dest) (list 'push 'p dest))
(mkcall (fn) (list 'call fn))
(mklink (n)
  (cond
   ((eq n 1) nil)
   (t (cons (mkmove n 1) (mklink1 (sub1 n))))))
(mklink1 (n)
  (cond
   ((zerop n) nil)
   (t (cons (mkpop n) (mklink1 (sub1 n))))))
(mkpop (n) (list 'pop 'p n))
(mkmove (dest val) (list 'move dest val))
)
