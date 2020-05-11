(df pp funs
    (mapc funs 
          (lambda (name)
            (pp1 (list name
                       (getrep (eval name))) 0)))
    (terpri)
    funs)

(de pprint (exp)
  (pp1 exp 0)
  (terpri)
  ())

(de pp1 (exp indent)
  (cond
   ((listp exp)
    (prin1 "(")
    (ppl exp indent))
   (t (prin2 exp)))
 ())

(de ppl (exp indent)
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
  ())

(de ppnl (indent)
  (terpri)
  (spaces indent))

(de ppl1 (exp indent tmp)
  (cond
   ((memb (car exp) '(lambda nlambda while))
    (prin2 (car exp))
    (pplam (cdr exp) indent))
   ((memb (car exp) '(cond closure))
    (prin2 (car exp))
    (ppcond (cdr exp) indent))
   (t (ppfun exp indent))))
  
(de pplam (exp indent)
  (prin1 " ")
  (cond
   ((car exp)
    (prin2 (car exp)))
   (t (prin1 "()")))
  (ppnl (setq indent (add1 indent)))
  (pptail (cdr exp) indent))

(de ppcond (exp indent)
  (ppnl (setq indent (add1 indent)))
  (pptail exp indent))

(de ppfun (exp indent)
  (prin2 (car exp))
  (and (cdr exp) (prin1 " "))
  (pptail (cdr exp) (+ indent
                       (symlen (car exp))
                       2)))

(de symlen (s)
  (strlen (symstr s)))

(de pptail (exp indent)
  (cond
   ((and (litatom (car exp))
         (lessp (length (cadr exp))
	        3)
         (lessp (length exp) 3))
    (prin2 (car exp))
    (setq indent
          (+ indent
             (symlen (car exp))
             1))
    (and (setq exp (cdr exp))
         (prin1 " "))))
  (while exp
   (pp1 (car exp) indent)
   (and (cdr exp) (ppnl indent))
   (setq exp (cdr exp))))
