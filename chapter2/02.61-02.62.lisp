
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

;;   *Exercise 2.61:* Give an implementation of `adjoin-set' using the
;;   ordered representation.  By analogy with `element-of-set?' show
;;   how to take advantage of the ordering to produce a procedure that
;;   requires on the average about half as many steps as with the
;;   unordered representation.

;; recursive
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; iterative
(define (adjoin-set-iter x set)
  (define (helper left x right)
    (cond ((null? right) (append left (list x)))
          ((= x (car right)) (append left right))
          ((< x (car right)) (append left (cons x right)))
          (else (helper (append left (list (car right))) x (cdr right)))))
  (helper null x set))

;; abominable, I know! And definitely not in 1/2 time...

(define null '()) 

(adjoin-set 3 null)
(adjoin-set-iter 3 null)
;; (3)
(adjoin-set 3 '())
(adjoin-set-iter 3 '())
;; (3)
(adjoin-set 3 '(3 4 5)) 
(adjoin-set-iter 3 '(3 4 5))
;; (3 4 5)
(adjoin-set 3 '(4 5 6))
(adjoin-set-iter 3 '(4 5 6))
;; (3 4 5 6)
(adjoin-set 3 '(1 2 4 5))
(adjoin-set-iter 3 '(1 2 4 5))
;; (1 2 3 4 5)
(adjoin-set 3 '(0 1 2))
(adjoin-set-iter 3 '(0 1 2))
;; (0 1 2 3)


;;   *Exercise 2.62:* Give a [theta](n) implementation of `union-set'
;;   for sets represented as ordered lists.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))
        (else
         (cons (car set1) (union-set (cdr set1) (cdr set2))))))
        
;; A merge, for any merge-sort fans in the house.
