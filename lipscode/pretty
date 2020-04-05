# pretty -- lips pretty printer
#
#   New improved pretty-printer.  Prints super parenthesis if %rpars is
# numeric (note that read can't handle supers yet). Tries to cram as many atoms
# as possible on a line. Long lists are sometimes indented incorrectly.
#
# Copyright 1989, Krister Joas
#
# $Id$
#
(setq %rpars nil)

(df pp funs
    (mapc funs (lambda (name)
		 (terpri)
	      	 (if (memb (typeof (eval name)) '(subr fsubr))
                     (cpprint name)
                     (pprint (list name (getrep (eval name)))))))
    funs)

(de pprint (exp)
 (pp1 exp 0)
    (terpri)
    nil)

(de pp1 (exp indent super)
    (cond
      ((listp exp)
	(cond
	  ((and (numberp %rpars)
                (greaterp (depth exp)
			  %rpars)
		(not super))
	    (prin1 "[")
	    (ppl exp indent t)
	    (prin1 "]"))
	  (t (prin1 "(")
	     (ppl exp indent super))))
      (t (prin2 exp)))
    nil)

(de ppl
  (exp indent super)
    (cond
      ((atom (car exp))
	(ppl1 exp indent super))
      (t (setq indent (add1 indent))
	 (pp1 (car exp)
	       indent
	       (and super (not (cdr exp))))
	 (setq exp (cdr exp))
	 (and exp (progn (ppnl indent)
			 (pptail exp indent super)))))
    (and (not super)
	 (prin1 ")"))
    nil)

(de ppl1
  (exp indent super tmp)
    (cond
      ((memb (car exp)
	     (quote (lambda nlambda while)))
	(prin2 (car exp))
	(pplam (cdr exp)
		indent super))
      ((memb (car exp)
	     (quote (cond closure)))
	(prin2 (car exp))
	(ppcond (cdr exp)
		 indent super))
      (t (ppfun exp indent super))))

(de ppcond
  (exp indent)
    (ppnl (setq indent (add1 indent)))
    (pptail exp indent super))

(de pplam
  (exp indent)
    (prin1 " ")
    (cond
      ((car exp)
	(prin2 (car exp)))
      (t (prin1 "()")))
    (ppnl (setq indent (add1 indent)))
    (pptail (cdr exp)
	     indent super))

(de ppfun
  (exp indent)
    (prin2 (car exp))
    (and (cdr exp)
	 (prin1 " "))
    (pptail (cdr exp)
	     (+ indent (symlen (car exp))
		   2)
	     super))

(de pptail
  (exp indent super)
    (cond
      ((null exp))
      ((and (litatom (car exp))
	    (lessp (length (cadr exp))
		   3)
	    (lessp (length exp)
		   3))
	(setq indent (+ indent (symlen (car exp))
			   1))
	(prin2 (car exp))
	(and (setq exp (cdr exp))
	     (prin1 " "))))
    (pptail1 exp indent super))

(de pptail1
  (exp indent super indent2)
  (setq indent2 indent)
  (while exp
    (pp1 (car exp)
         indent2
         (and super (not (cdr exp))))
    (and (cdr exp)
         (cond
          ((and (litatom (car exp))
                (setq indent2
                      (+ indent2 (symlen (car exp)) 1))
                (if (greaterp indent2 60)
                    (lessp (+ indent2
                              (if (litatom (cadr exp))
                                  (symlen (cadr exp))
                                0)) 79)
                  t))
           (prin1 " "))
          (t (ppnl indent)
             (setq indent2 indent))))
    (setq exp (cdr exp))))

(de ppnl
  (indent)
    (terpri)
    (spaces indent))

(de symlen (s)
  (strlen (symstr s)))

(de depth
  (l)
    (cond
      ((nlistp l)
	0)
      ((null l)
	0)
      ((null (cdr l))
	(add1 (depth (car l))))
      (t (depth (cdr l)))))
