#
# A sort tree sort routine.
#
# $Id$
#
(defineq
  (sortlist
   (lambda (l sorted tmp l1)
     (while (not sorted)
       (setq sorted t)
       (setq l1 l)
       (while (cdr l1)
         (cond ((greaterp (strcmp (symstr (car l1))
                                  (symstr (cadr l1)))
                          0)
                (setq tmp (car l1))
                (rplaca l1 (cadr l1))
                (rplaca (cdr l1)
                        tmp)
                (setq sorted)))
         (setq l1 (cdr l1))))
     l))

  (treesort
   (lambda (l)
     (traverse (build l))))

  (traverse
   (lambda (tree)
     (cond
       ((null tree) nil)
       (t (append
           (traverse (cadr tree))
           (list (car tree))
           (traverse (caddr tree)))))))

  (build
   (lambda (l tree)
     (while l
       (setq tree (insert (car l) tree))
       (setq l (cdr l)))
     tree))

  (insert
   (lambda (elt tree)
     (cond
       ((null tree) (makenode elt))
       ((greaterp (strcmp (symstr elt) (symstr (car tree))) 0)
        (makenode (car tree) (cadr tree) (insert elt (caddr tree))))
       (t (makenode (car tree) (insert elt (cadr tree)) (caddr tree))))))

  (makenode
   (lambda (elt left right)
     (cons elt (cons left (cons right)))))
  )
