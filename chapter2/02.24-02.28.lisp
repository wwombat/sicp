
;;   *Exercise 2.24:* Suppose we evaluate the expression `(list 1 (list
;;   2 (list 3 4)))'.  Give the result printed by the interpreter, the
;;   corresponding box-and-pointer structure, and the interpretation of
;;   this as a tree (as in *Note Figure 2-6::).

;;   *Exercise 2.25:* Give combinations of `car's and `cdr's that will
;;   pick 7 from each of the following lists:

;;        (1 3 (5 7) 9)
;;        ((7))
;;        (1 (2 (3 (4 (5 (6 7))))))

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))) 
(car (car '((7)))) 
(cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7)))))))))))) 

;; Pretty straight foward!
;; Now, how might we write a program that mechanically searches for
;; such sequences of operations? :3

;; For the purposes of this exercise, a brute force approach suffices!

(define (search curr goal path)
  (display curr) (newline)
  (display (pair? curr)) (newline)
  (cond  ((null? curr) nil)
         ((pair? curr)
          (get-first-not-null (list ;; the crux is here! we are performing an exhaustive
                                    ;; search!
                               (search (car curr) goal (append path (list 'car)))
                               (search (cdr curr) goal (append path (list 'cdr))))))
         ((= goal curr) path)
         (else nil)))

(define (search-list pred x)
  (display "hi")
  (define (helper l) (cond ((null? l) nil)
                           ((pred (car l)) (car l))
                           (else (helper (cdr l)))))
    (helper x)) 

(define (get-first-not-null x)
  (search-list (lambda (x) (not (null? x))) x)) 

(search '(1 3 (5 7) 9) 7 nil) 
(search '((7)) 7 nil)
(search '(1 (2 (3 (4 (5 (6 7)))))) 7 nil) 

;; Admittedly, this is not the most pretty approach- and,
;; in fact, it could get easily fried (like, admittedly, most
;; of the list functions we have examined thus far!)
;; by CYCLES (god forbid!)

;; One optimization would be to write the search such that it
;; terminates upon finding the answer!
;; One could take both a breadth first or a depth first approach
;; to such an optimization.

;; An example depth first search :

(define (search-early-termination curr goal path)
  (display curr) (newline)
  (display (pair? curr)) (newline)
  (cond  ((null? curr) nil)
         ((pair? curr)
          (let ((car-half (search-early-termination (car curr) goal (append path (list 'car)))))
            (cond ((not (null? car-half)) car-half)
                  (else (search-early-termination (cdr curr) goal (append path (list 'cdr)))))))
         ((= goal curr) path)
         (else nil)))

(search '(1 3 (5 7) 9) 7 nil) 

(search-early-termination '(1 3 (5 7) 9) 7 nil) 

;;   *Exercise 2.26:* Suppose we define `x' and `y' to be two lists:

          (define x (list 1 2 3))

          (define y (list 4 5 6))

;;   What result is printed by the interpreter in response to
;;   evaluating each of the following expressions:

          (append x y)

;; (1 2 3 4 5 6)

          (cons x y)

;; ((1 2 3) 4 5 6)

          (list x y)

;; ((1 2 3) (4 5 6))

;;   *Exercise 2.27:* Modify your `reverse' procedure of *Note Exercise
;;   2-18:: to produce a `deep-reverse' procedure that takes a list as
;;   argument and returns as its value the list with its elements
;;   reversed and with all sublists deep-reversed as well.  For example,

          (define x (list (list 1 2) (list 3 4)))
          (define y (list (list 1 2) (list 3 4) 4 3 (list 1 3 4 5 6)))
          (define z (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))

          x
;;        ((1 2) (3 4))

          (reverse x)
;;        ((3 4) (1 2))

          (deep-reverse x)
;;        ((4 3) (2 1))

          (deep-reverse y)
          (deep-reverse z)

;;  (define (reverse a)
;;  (cond ((null? a) 'nil)
;;          ((null? (cdr a)) (list (car a)))
;;          ((= 1 1) (append (reverse (cdr a)) (list (car a))))))

(define (deep-reverse a)
  (cond ((null? a) 'nil)
        ((not (pair? a)) a) 
        ((null? (cdr a)) (list (deep-reverse (car a))))
        ((= 1 1) (append (deep-reverse (cdr a)) (list (deep-reverse (car a)))))))

          (deep-reverse y)

;;   *Exercise 2.28:* Write a procedure `fringe' that takes as argument
;;   a tree (represented as a list) and returns a list whose elements
;;   are all the leaves of the tree arranged in left-to-right order.
;;   For example,

          (define x (list (list 1 2) (list 3 4)))

          (fringe x)
          ;;(1 2 3 4)

          (fringe (list x x))
          ;;(1 2 3 4 1 2 3 4)

(define nil '()) 

(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append
               (fringe (car x))
               (fringe (cdr x))))))
