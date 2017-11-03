;;   *Exercise 2.29:* A binary mobile consists of two branches, a left
;;   branch and a right branch.  Each branch is a rod of a certain
;;   length, from which hangs either a weight or another binary mobile.
;;   We can represent a binary mobile using compound data by
;;   constructing it from two branches (for example, using `list'):

(define (make-mobile left right)
  (list left right))

;;   A branch is constructed from a `length' (which must be a number)
;;   together with a `structure', which may be either a number
;;   (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;;     a. Write the corresponding selectors `left-branch' and
;;        `right-branch', which return the branches of a mobile, and
;;        `branch-length' and `branch-structure', which return the
;;        components of a branch.

(define (left-branch mobile)
  (car mobile)) 

(define (right-branch mobile)
  (cadr mobile)) 

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;;     b. Using your selectors, define a procedure `total-weight' that
;;        returns the total weight of a mobile.

;; What might initially seem like a very straightfoward problem
;; is made complicated by our choice of representation above!
;; while in the structures we have seen before, the 'left' and 'right'
;; branches have returned structures of essentially the same 'type',
;; the representation we have chosen for a mobile returns *alternating*
;; types!

;; Calling '{left,right}branch' on a mobile returns a 'branch' structure,
;; while calling 'branch-structure' on a branch returns a mobile!

;; With this alternation present in the structure of our representation,
;; it is natural to create a solution that alternates between two courses
;; of action as well, each represented here by a separate procedure.

;; Unfortunately, we have not been provided the luxury of test data
;; for this problem, so we must manufacture our own!

(define my-mobile-a
  (make-mobile
   (make-branch 2 5)
   (make-branch 1 10))) 

(define my-mobile-b
  (make-mobile
   (make-branch 15 1)
   (make-branch 1 my-mobile-a))) 

(define my-mobile-c
  (make-mobile
   (make-branch 1 my-mobile-a)
   (make-branch 1 my-mobile-b)))

(define (weight branch)
  (let ((structure (branch-structure branch)))
    (cond
     ((not (pair? structure)) structure)
     (else (total-weight structure)))))

(weight (left-branch my-mobile-a)) 
(weight (right-branch my-mobile-a)) 

(define (total-weight mobile)
  (if (pair? mobile)
      (+ (weight (left-branch mobile))
         (weight (right-branch mobile)))
    mobile))

(total-weight my-mobile-a)
;; 15
(total-weight my-mobile-b)
;; 16
(total-weight my-mobile-c)
;; 31

;; That was fun!

;;     c. A mobile is said to be "balanced" if the torque applied by
;;        its top-left branch is equal to that applied by its top-right
;;        branch (that is, if the length of the left rod multiplied by
;;        the weight hanging from that rod is equal to the
;;        corresponding product for the right side) and if each of the
;;        submobiles hanging off its branches is balanced. Design a
;;        predicate that tests whether a binary mobile is balanced.

;; length x weight

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (is-balanced? mobile)
  (if (not (pair? mobile)) #t
    (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
         (is-balanced? (branch-structure (left-branch mobile)))
         (is-balanced? (branch-structure (right-branch mobile))))))

(torque (left-branch my-mobile-a)) 

(torque (left-branch my-mobile-a)) 

(torque (left-branch my-mobile-b)) 

(torque (right-branch my-mobile-b)) 

(is-balanced? my-mobile-a) 

(is-balanced? my-mobile-b) 

(is-balanced? my-mobile-c) 

;;     d. Suppose we change the representation of mobiles so that the
;;        constructors are

             (define (make-mobile2 left right)
               (cons left right))

             (define (make-branch2 length structure)
               (cons length structure))

;;        How much do you need to change your programs to convert to
;;        the new representation?

;; To answer this, we should note the difference between evaluating
;; (list 1 2) and (const 1 2), which produce (1 . (2 . nil)) and (1 . 2)
;; respectively!

;; In the above code, note the 'cadr' calls used to access the second
;; item in each structure (i.e., right-branch / branch-structure).
;; To convert to the new representation, it suffices to switch out
;; 'cadr' for a good old 'cdr' call.
