#
# A sort tree sort routine.
#
# $Id$
#
(de sortlist (l sorted tmp l1)
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
    l)

(de treesort (l)
    (traverse (build l)))

(de traverse (tree)
    (cond
     ((null tree) nil)
     (t (append
         (traverse (cadr tree))
         (list (car tree))
         (traverse (caddr tree))))))

(de build (l tree)
    (while l
      (setq tree (insert (car l) tree))
      (setq l (cdr l)))
    tree)

(de insert (elt tree)
    (cond
     ((null tree) (makenode elt))
     ((greaterp (strcmp (symstr elt) (symstr (car tree))) 0)
      (makenode (car tree) (cadr tree) (insert elt (caddr tree))))
     (t (makenode (car tree) (insert elt (cadr tree)) (caddr tree)))))

(de makenode (elt left right)
    (cons elt (cons left (cons right))))
