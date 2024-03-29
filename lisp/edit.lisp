#
# edit -- structure editor for lips
#
# Copyright 1989, 2020, 2022 Krister Joas.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

(defineq
  (copy
   (lambda (exp nexp)
     (setq nexp (cons))
     (cond
       ((atom exp) exp)
       (t (while (listp exp)
            (tconc nexp (copy (car exp)))
            (setq exp (cdr exp)))
          (car nexp)))))

  (edit
   (nlambda (fun)
     (let ((eexp (getrep (eval fun)))
           oldexp cexp clist (editing t)
           com tmp (plev (printlevel 3)))
       (setq oldexp (copy eexp))
       (setq cexp eexp)
       (while editing
         (prin1 "*")
         (setq com (readline))
         (editcom))
       (list fun))))

  (editcom
   (lambda ()
     (while com
       (setq tmp (car com))
       (cond
         ((null tmp))
         ((and (litatom tmp)
               (getprop tmp 'editcom))
          ((getprop tmp 'editcom)))
         ((zerop tmp)
          (cond
            (clist (setq cexp (car clist))
                   (setq clist (cdr clist)))))
         ((numberp tmp)
          (cond
            ((greaterp (abs tmp) (length cexp))
             (print (list tmp '?))
             (setq com))
            ((minusp tmp)
             (setq clist (attach cexp clist))
             (setq cexp (car (nth cexp (plus (length cexp) tmp 1)))))
            (t (setq clist (attach cexp clist))
               (setq cexp (car (nth cexp tmp))))))
         ((listp tmp)
          (cond
            ((numberp (car tmp))
             (numedit cexp tmp))
            ((and (litatom (car tmp))
                  (getprop (car tmp) 'editcom2))
             ((getprop (car tmp) 'editcom2)
              (cdr tmp)))
            (t (print (list (car tmp) '?))
               (setq com))))
         (t (print (list tmp '?))
            (setq com)))
       (setq com (cdr com)))))

  (numedit
   (lambda (cexp com tmp)
     (setq tmp (car com))
     (cond
       ((or (nlistp cexp)
            (null cexp))
        (print '?))
       ((null (cdr com))
        (cond
          ((minusp tmp)
           (print (list tmp '?)))
          ((eq tmp 1)
           (rplaca cexp (cadr cexp))
           (rplacd cexp (cddr cexp)))
          (t (while (greaterp tmp 2)
               (setq cexp (cdr cexp))
               (setq tmp (sub1 tmp)))
             (if (not cexp)
                 (print '?)
                 (rplacd cexp (cddr cexp))))))
       ((eq tmp -1)
        (setq tmp (car cexp))
        (rplaca cexp (cadr com))
        (rplacd cexp (nconc (cddr com) (list tmp) (cdr cexp))))
       ((minusp tmp)
        (while (lessp tmp -2)
          (setq cexp (cdr cexp))
          (setq tmp (add1 tmp)))
        (if (cdr cexp)
            (rplacd cexp (nconc (cdr com) (cdr cexp)))
            (print (list (car com) '?))))
       ((eq tmp 1)
        (rplaca cexp (cadr com))
        (rplacd cexp (nconc (cddr com) (cdr cexp))))
       (t (while (greaterp tmp 2)
            (setq cexp (cdr cexp))
            (setq tmp (sub1 tmp)))
          (if (cdr cexp)
              (rplacd cexp (nconc (cdr com) (cddr cexp)))
              (print (list (car com) '?)))))))
  )

(putprop 'p 'editcom
         (lambda ()
           (print cexp)))

(putprop 'pp 'editcom
         (lambda ()
           (pprint cexp)))

(putprop '? 'editcom
         (lambda ()
           (printlevel plev)
           (print cexp)
           (printlevel 3)))

(putprop '^ 'editcom
         (lambda (cexp)
           (setq clist)
           (setq cexxp eexp)))

(putprop 'stop 'editcom
         (lambda ()
           (setq editing)))

(putprop 'ok 'editcom
         (lambda ()
           (setq editing)
           (if (not (equal eexp oldexp))
               (print (list fun 'redefined)))
           (putprop fun 'olddef (eval oldexp))
           (set fun (eval eexp))))

(putprop 'undo! 'editcom
         (lambda (tmp)
           (print 'Undone.)
           (set fun (eval oldexp))
           (setq clist)
           (setq tmp oldexp)
           (setq oldexp eexp)
           (setq eexp tmp)
           (setq cexp tmp)))

(putprop 'n 'editcom2
         (lambda (arg)
           (nconc cexp arg)))

(putprop 'li 'editcom2
         (lambda (arg)
           (setq arg (car arg))
           (cond
            ((eqp arg 1)
             (attach nil cexp)
             (rplaca cexp (cdr cexp))
             (rplacd cexp))
            ((numberp arg)
             (rplacd (nth cexp (sub1 arg))
                     (cons (nth cexp arg)))))))

(putprop 'bo 'editcom2
         (lambda (arg)
           (setq arg (car arg))
           (cond
            ((eqp arg 1)
             (rplacd (nconc (car cexp) (cdr cexp)))
             (rplaca (cdr cexp))
             (rplacd cexp))
            ((numberp arg)
             (cond
              ((nlistp (car (nth cexp arg))))
              (t (nconc (car (nth cexp arg))
                        (nth cexp (add1 arg)))
                 (rplacd (nth cexp (sub1 arg))
                         (car (nth cexp arg)))))))))
