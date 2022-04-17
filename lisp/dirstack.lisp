#
# dirstack -- directory stack functions
# Copyright 1989, 2020 Krister Joas.
#
# $Id$
#

# "advise" cd, sort of.
(if (null (getprop 'cd 'origcd))
    (progn
      (putprop 'cd 'origcd cd)
      (setq origcd cd)))

(defineq
  (cd
   (nlambda (dir)
     (if (not (apply* 'origcd dir t))
         (print '(no such directory))
         (rplaca dirstack (getenv PWD))
         (putprop 'dirstack 'dirstack (prettydirs dirstack)))))

  (pushd
   (nlambda (dir)
     (cond ((null dir) (pushd1 1))
	   ((numberp dir) (pushd1 dir))
	   ((atom dir)
	    (if (setq dir (expand dir t))
                (if (cdr dir)
                    (print '(not unique))
                    (pushd2 (symstr (car dir))))))
	   ((stringp dir) (pushd2 dir))
	   (t (print '(illegal arg))))
     nil))

  (pushd1
   (lambda (n ds)
     (setq ds dirstack)
     (while (not (zerop n))
       (setq ds (cdr ds))
       (setq n (sub1 n)))
     (cond
       ((null ds) (print '(directory stack empty)))
       ((null (apply* 'origcd (car ds) t))
        (print '(no such directory)))
       (t (setq n (car ds))
          (rplaca ds (car dirstack))
          (rplaca dirstack n)
          (dirsprint dirstack t)))
     nil))

  (pushd2
   (lambda (dir)
     (if (null (apply* 'origcd dir t))
         (print '(no such directory))
         (setq dirstack (cons (getenv PWD) dirstack))
         (dirsprint dirstack t))
     nil))

  (popd
   (lambda ()
     (cond ((cdr dirstack)
            (if (null (apply* 'origcd (cadr dirstack)))
                (print '(no such directory))
                (setq dirstack (cdr dirstack))
                (dirsprint dirstack t)))
           (t (print '(directory stack empty))))
     nil))

  (prettydirs
   (lambda (ds)
     (mapcar ds
             (lambda (x)
               (cond
                 ((streq home x) "~")
                 ((streq home
                         (cond
                           ((substr x 1 (strlen home)))
                           (t "")))
                  (concat "~" (substr x (plus (strlen home) 1) (strlen x))))
                 (t x))))))

  (dirsprint
   (lambda (ds save)
     (if save
         (putprop 'dirstack 'dirstack (prin1 (prettydirs ds)))
         (prin1 (getprop 'dirstack 'dirstack)))
     (terpri)))

  (dirs
   (lambda ()
     (dirsprint dirstack)))

  (pwd
   (lambda ()
     (prin1 (car dirstack))
     (terpri)))
  )

(setq dirstack (cons (getenv PWD)))
(putprop 'dirstack 'dirstack
         (prettydirs dirstack))
