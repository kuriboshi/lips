(de rev (l)
 (cond
  ((null l)
   nil)
  ((null (cdr l))
   l)
  (t (cons (car (rev (cdr l)))
           (rev (cons (car l)
                      (rev (cdr (rev (cdr l))))))))))
