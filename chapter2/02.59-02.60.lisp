
     (define (element-of-set? x set)
       (cond ((null? set) false)
             ((equal? x (car set)) true)
             (else (element-of-set? x (cdr set)))))

     (define (adjoin-set x set)
       (if (element-of-set? x set)
           set
           (cons x set)))

     (define (intersection-set set1 set2)
       (cond ((or (null? set1) (null? set2)) '())
             ((element-of-set? (car set1) set2)
              (cons (car set1)
                    (intersection-set (cdr set1) set2)))
             (else (intersection-set (cdr set1) set2))))

 ;;  *Exercise 2.59:* Implement the `union-set' operation for the
 ;;  unordered-list representation of sets.

;; iterative
(define (union-set input output)
  (cond ((null? input) output)
        (else (union-set (cdr input) (adjoin-set (car input) output)))))

;; recursive
(define (union-set-rec set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set-rec (cdr set1) set2))
        (else (cons (car set1)
                    (union-set-rec (cdr set1) set2)))))

;; What are the performance characteristics of these implementations?
;; The iterative solution performs exactly len(input) number of
;; (adjoin-set x output) calls, each of which takes time proportional
;; to the length of output (since adjoin compares the new element against
;; each element of the existing set). This gives it a time complexity
;; proportional to len(input) * len(output)

;; The recursive implementation performs identically, running
;; element-of-set on set2 len(set1) number of times.

(define set-a '(1 2 3))

(define set-b '(1 3 5))

(intersection-set set-a set-b) 

(union-set set-a set-b) 
(union-set set-b set-a)
(union-set set-b set-b)
(union-set-rec set-a set-b) 
(union-set-rec set-b set-a) 
(union-set-rec set-b set-b) 
(union-set-rec '() '(1 2 3)) 
(union-set-rec '(0 1 2 19) '(1 2 3)) 

 ;;  *Exercise 2.60:* We specified that a set would be represented as a
 ;;  list with no duplicates.  Now suppose we allow duplicates.  For
 ;;  instance, the set {1,2,3} could be represented as the list `(2 3 2
 ;;  1 3 2 2)'.  Design procedures `element-of-set?', `adjoin-set',
 ;;  `union-set', and `intersection-set' that operate on this
 ;;  representation.  How does the efficiency of each compare with the
 ;;  corresponding procedure for the non-duplicate representation?  Are
 ;;  there applications for which you would use this representation in
 ;;  preference to the non-duplicate one?

     (define (element-of-dup-set? x set)
       (cond ((null? set) false)
             ((equal? x (car set)) true)
             (else (element-of-set? x (cdr set)))))

     (define (adjoin-dup-set x set)
       (cons x set))

     (define (intersection-dup-set set1 set2)
       (cond ((or (null? set1) (null? set2)) '())
             ((element-of-dup-set? (car set1) set2)
              (cons (car set1)
                    (intersection-dup-set (cdr set1) set2)))
             (else (intersection-dup-set (cdr set1) set2))))

     (define (union-dup-set input output)
       (append input output))

;; This implementation has a sizable benefit when adding
;; elements to the set - adding a single element occurs in
;; constant (!) time, and performing a union requires time
;; proportional to the length of the smaller set (due to the
;; cost of the append call - if we were to permit mutation,
;; this operation would be constant time as well!)
;;
;; The speed up for insertions and unions, however, comes
;; at a cost- element-of-set? now takes an amount of time
;; equal to the total number of items in the unordered list
;; - which, depending on how that set was constructed,
;; and how many union and adjoin operations have been
;; performed on it, may be substantially more larger than
;; the actual size of the set! This solution also requires,
;; of course, more memory per set than the first implementation.
;;
;; This implementation should be favored when vastly more
;; unions and adjoinments are performed that intersections
;; and lookups.
;;
