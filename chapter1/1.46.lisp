
;;   *Exercise 1.46:* Several of the numerical methods described in
;;   this chapter are instances of an extremely general computational
;;   strategy known as "iterative improvement".  Iterative improvement
;;   says that, to compute something, we start with an initial guess
;;   for the answer, test if the guess is good enough, and otherwise
;;   improve the guess and continue the process using the improved
;;   guess as the new guess.  Write a procedure `iterative-improve'
;;   that takes two procedures as arguments: a method for telling
;;   whether a guess is good enough and a method for improving a guess.
;;   `Iterative-improve' should return as its value a procedure that
;;   takes a guess as argument and keeps improving the guess until it
;;   is good enough.  Rewrite the `sqrt' procedure of section *Note
;;   1-1-7:: and the `fixed-point' procedure of section *Note 1-3-3::
;;   in terms of `iterative-improve'.

;;   (define (sqrt-iter guess x)
;;     (if (good-enough? guess x)
;;         guess
;;         (sqrt-iter (improve guess x)
;;                    x)))

;;   (define (improve guess x)
;;     (average guess (/ x guess)))

;;   (define (average x y)
;;     (/ (+ x y) 2))

;;   (define (good-enough? guess x)
;;     (< (abs (- (square guess) x)) 0.001))

;;   (define (sqrt x)
;;     (sqrt-iter 1.0 x))

(define (iterative-improve good-enough? improve)
  (define iter (lambda (guess)
                 (if (good-enough? guess) guess
                   (iter (improve guess)))))
  iter) 

(define (sqrt x)
   (define (improve guess)
     (average guess (/ x guess)))
   (define (average x y)
     (/ (+ x y) 2))
   (define (good-enough? guess)
     (< (abs (- (square guess) x)) 0.001))
   ((iterative-improve good-enough? improve) 1.0)) 

(define (sqrt x)
  (let ((improve (lambda (guess) (average guess (/ x guess))))
        (average (lambda (x y) (/ (+ x y) 2)))
        (good-enough? (lambda (guess) (< (abs (- (square guess) x)) 0.001))))
    ((iterative-improve good-enough? improve) 1.0)))

(define (fixed-point f first-guess)
  (let (
        (tolerance 0.00001)
        (good-enough? (lambda (guess)
                       (< (abs (- guess (f guess))) tolerance)))
        )
    ((iterative-improve good-enough? f) 1.0))) 

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(newtons-method (cubic 4 8 3) -100)
