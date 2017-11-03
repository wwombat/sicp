

;;   *Exercise 2.4:* Here is an alternative procedural representation
;;   of pairs.  For this representation, verify that `(car (cons x y))'
;;   yields `x' for any objects `x' and `y'.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; Let's examine through the magic of substitution

(define x (cons 'a 'b))
;; x := (lambda (m) (m 'a 'b))

(car x) 
;; (x (lambda (p q) p))
;; ((lambda (p q) p) 'a 'b)
;; 'b

;;   What is the corresponding definition of `cdr'? (Hint: To verify
;;   that this works, make use of the substitution model of section
;;   *Note 1-1-5::.)

(define (cdr z)
  (z (lambda (p q) q))) 

(cdr x) 
