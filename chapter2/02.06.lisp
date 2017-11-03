
;;   *Exercise 2.6:* In case representing pairs as procedures wasn't
;;   mind-boggling enough, consider that, in a language that can
;;   manipulate procedures, we can get by without numbers (at least
;;   insofar as nonnegative integers are concerned) by implementing 0
;;   and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;   This representation is known as "Church numerals", after its
;;   inventor, Alonzo Church, the logician who invented the [lambda]
;;   calculus.

;;   Define `one' and `two' directly (not in terms of `zero' and
;;   `add-1').  (Hint: Use substitution to evaluate `(add-1 zero)').
;;   Give a direct definition of the addition procedure `+' (not in
;;   terms of repeated application of `add-1').

;; Now, it is not immediately apparent what is meant by these procedures
;; 'implementing' numbers!

;; However, they seem like smart peeps, so I will trust them.
;; How about we give these functions a spin and see what they
;; do!

(add-1 zero) 
(lambda (f) (lambda (x) (f ((zero f) x)))) 
(lambda (f) (lambda (x) (f ((lambda (f) f) x))))
(lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; let's call that one.

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define three (lambda (f) (lambda (x) (f (f (f x))))))

;; aha!

(define (plus a b)       ;; apply a f's to x with b f's applied
  (lambda (f) (lambda (x) ((a f) ((b f) x))))) 

(define (inc x) (+ 1 x)) 

(((plus three two) inc) 0) 

(define five (plus three two)) 


;; For extra credit!

(define (mult a b)
  (lambda (f) (a (b f))))

