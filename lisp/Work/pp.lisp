(defineq
    (pp
     (nlambda funs
              (mapc funs
                    (lambda (name)
                      (pp1 (list name (getrep (eval name))) 0)))
              (terpri)
              funs))

    (pprint
     (lambda (exp)
       (pp1 exp 0)
       (terpri)
       ()))

  (pp1
   (lambda (exp indent)
     (cond
       ((listp exp)
        (prin1 "(")
        (ppl exp indent))
       (t (prin2 exp)))
     ()))

  (ppl
   (lambda (exp indent)
     (cond
       ((atom (car exp))
        (ppl1 exp indent))
       (t (setq indent
                (add1 indent))
          (pp1 (car exp)
               indent)
          (setq exp (cdr exp))
          (and exp
               (progn (ppnl indent)
                      (pptail exp indent)))))
     (prin1 ")")
     ()))

  (ppnl
   (lambda (indent)
     (terpri)
     (spaces indent)))

  (ppl1
   (lambda (exp indent tmp)
     (cond
       ((memb (car exp) '(lambda nlambda while))
        (prin2 (car exp))
        (pplam (cdr exp) indent))
       ((memb (car exp) '(cond closure))
        (prin2 (car exp))
        (ppcond (cdr exp) indent))
       (t (ppfun exp indent)))))

  (pplam
   (lambda (exp indent)
     (prin1 " ")
     (cond
       ((car exp)
        (prin2 (car exp)))
       (t (prin1 "()")))
     (ppnl (setq indent (add1 indent)))
     (pptail (cdr exp) indent)))

  (ppcond
   (lambda (exp indent)
     (ppnl (setq indent (add1 indent)))
     (pptail exp indent)))

  (ppfun
   (lambda (exp indent)
     (prin2 (car exp))
     (and (cdr exp) (prin1 " "))
     (pptail (cdr exp) (plus indent
                             (symlen (car exp))
                             2))))

  (symlen
   (lambda (s)
     (strlen (symstr s))))

  (pptail
   (lambda (exp indent)
     (cond
       ((and (litatom (car exp))
             (lessp (length (cadr exp))
	            3)
             (lessp (length exp) 3))
        (prin2 (car exp))
        (setq indent
              (plus indent
                    (symlen (car exp))
                    1))
        (and (setq exp (cdr exp))
             (prin1 " "))))
     (while exp
       (pp1 (car exp) indent)
       (and (cdr exp) (ppnl indent))
       (setq exp (cdr exp)))))
  )
