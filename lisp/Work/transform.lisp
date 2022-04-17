(defineq
    (transform
     (lambda (exp tmp)
       (setq tmp exp)
       (while tmp
         (cond
           ((eq (cadr tmp)
                (quote *))
            (setq exp (cons (quote PIPE)
                            (cons exp (cons (transform (cddr tmp))))))
            (rplacd tmp nil)
            (setq tmp))
           (t (setq tmp (cdr tmp)))))
       exp))
    )
