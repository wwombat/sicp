;;   *Exercise 2.30:* Define a procedure `square-tree' analogous to the
;;   `square-list' procedure of *Note Exercise 2-21::.  That is,
;;   `square-list' should behave as follows:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
        ;;(1 (4 (9 16) 25) (36 49))

;;   Define `square-tree' both directly (i.e., without using any
;;   higher-order procedures) and also by using `map' and recursion.

(map (lambda (x) (* x 2)) (list 1 2 3 4)) 
(map square (list 1 2 3 4)) 

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))))) 

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (cond ((pair? sub-tree) (square-tree-2 sub-tree))
               (else (square sub-tree))))
       tree))

(square-tree-2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;;   *Exercise 2.31:* Abstract your answer to *Note Exercise 2-30:: to
;;   produce a procedure `tree-map' with the property that
;;   `square-tree' could be defined as

;;        (define (square-tree tree) (tree-map square tree))

(define (tree-map pred tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (pred tree))
        (else (cons (tree-map pred (car tree))
                    (tree-map pred (cdr tree)))))) 

;; Or, alternatively

(define (tree-map-2 pred tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map-2 pred sub-tree)
           (pred sub-tree)))
       tree))

(tree-map square (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(tree-map-2 square (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;;   *Exercise 2.32:* We can represent a set as a list of distinct
;;   elements, and we can represent the set of all subsets of the set as
;;   a list of lists.  For example, if the set is `(1 2 3)', then the
;;   set of all subsets is `(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2
;;   3))'.  Complete the following definition of a procedure that
;;   generates the set of subsets of a set and give a clear explanation
;;   of why it works:

(define (<??> x) (cons (car s) x)) 

(define (subsets s)
  (if (null? s)
      (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map
                    (lambda (x) (cons (car s) x))
                    rest))))) 

;; The key to understanding this solution is to grok
;; the inherently recursive nature of subsets - i.e.,
;; that the subsets of a sets subsets are also its subsets!

;; We can understand how the algorithm works by working in
;; reverse - while the algorithm works by peeling off items
;; from the list (as we can see from the call to cdr in the
;; recursive step), we can ask what would we would need to
;; ~add~ to a solution if we added a new item to the beginning
;; of the list.

;; Say that we have a list x, and already know all the subsets
;; of this list. Now, suppose that we have a new list y that 
;; we form by performing (define y (cons 'a x)).

;; Now, we can ask, which subsets of y are not also subsets
;; of x?
;; Or, imagining that we are 'building' up the answer, what
;; new values get 'added' when we prepended 'a to x?

;; It should be immediately apparent that all of x's subsets
;; are valid subsets of y- and equally apparent that all new
;; subsets are simply 'a appended to each existing subset of
;; x!

;; With that understanding under our belt, we have a straight-
;; forward way to read our recursive solution:

;; The subsets of any set are the union of the subsets of itself
;; with one element removed and and the set of those subsets
;; with that element appended to them. This reduction of
;; problem size eventually leads us to our base cases,
;; the empty list, which returns the empty list, which I suppose
;; is a subset of all sets.

