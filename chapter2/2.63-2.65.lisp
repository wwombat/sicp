     (define (entry tree) (car tree))

     (define (left-branch tree) (cadr tree))

     (define (right-branch tree) (caddr tree))

     (define (make-tree entry left right)
       (list entry left right))

     (define (element-of-set? x set)
       (cond ((null? set) false)
             ((= x (entry set)) true)
             ((< x (entry set))
              (element-of-set? x (left-branch set)))
             ((> x (entry set))
              (element-of-set? x (right-branch set)))))

 ;;  *Exercise 2.63:* Each of the following two procedures converts a
 ;;  binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

 ;;    a. Do the two procedures produce the same result for every tree?
 ;;       If not, how do the results differ?  What lists do the two
 ;;       procedures produce for the trees in *Note Figure 2-16::?



 ;;  *Figure 2.16:* Various binary trees that represent the set
 ;;  {1,3,5,7,9,11}.

 ;;          7          3             5
 ;;          /\         /\            /\
 ;;         3  9       1  7          3  9
 ;;        /\   \         /\        /   /\
 ;;       1  5  11       5  9      1   7  11
 ;;                          \
 ;;                          11



 ;;    b. Do the two procedures have the same order of growth in the
 ;;       number of steps required to convert a balanced tree with n
 ;;       elements to a list?  If not, which one grows more slowly?


 ;;  *Exercise 2.64:* The following procedure `list->tree' converts an
 ;;  ordered list to a balanced binary tree.  The helper procedure
 ;;  `partial-tree' takes as arguments an integer n and list of at
 ;;  least n elements and constructs a balanced tree containing the
 ;;  first n elements of the list.  The result returned by
 ;;  `partial-tree' is a pair (formed with `cons') whose `car' is the
 ;;  constructed tree and whose `cdr' is the list of elements not
 ;;  included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

 ;;    a. Write a short paragraph explaining as clearly as you can how
 ;;       `partial-tree' works.  Draw the tree produced by `list->tree'
 ;;       for the list `(1 3 5 7 9 11)'.

 ;;    b. What is the order of growth in the number of steps required by
 ;;       `list->tree' to convert a list of n elements?


 ;;  *Exercise 2.65:* Use the results of *Note Exercise 2-63:: and
 ;;  *Note Exercise 2-64:: to give [theta](n) implementations of
 ;;  `union-set' and `intersection-set' for sets implemented as
 ;;  (balanced) binary trees.(5)
