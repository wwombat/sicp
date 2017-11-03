

;;   *Exercise 2.5:* Show that we can represent pairs of nonnegative
;;   integers using only numbers and arithmetic operations if we
;;   represent the pair a and b as the integer that is the product 2^a
;;   3^b.  Give the corresponding definitions of the procedures `cons',
;;   `car', and `cdr'.

;; Since 2 and 3 are both primes, a prime factorization of 2^a * 3^b
;; will take the following form : a * a * a ... a * b * b * ... * b
;; To find the number of b's, we can simply iteratively divide out b's
;; until clean division by b is no longer possible. This is guaranteed
;; by the fact that they are relatively prime to each other -

;; assume there exists some positive x whereby (modulo a^x b) = 0
;; Note that, if modulo (a * c) b = 0, and a is not divisible by b, then
;; c must be divisible by b. Apply this in a recursive fashion to
;; a^x (i.e., a * a^(x - 1) etc.), and you get that a must be divisible
;; by b, contradiction central, RIP


(define (exp_cons a b) (* (expt 2 a) (expt 3 b))) 

(define (!= a b) (not (= a b))) 

(define (take_log base x)
  (define (take_log_helper base x counter)
    (if (not (= (modulo x base) 0))
        counter
      (take_log_helper base (/ x base) (+ 1 counter))))
  (take_log_helper base x 0)) 

(take_log 2 32) 

(define (exp_car pair) (take_log 2 pair)) 

(define (exp_cdr pair) (take_log 3 pair)) 

(exp_car (exp_cons 601 26)) 

(exp_cdr (exp_cons 3 27)) 
