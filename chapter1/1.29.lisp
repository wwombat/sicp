;;   *Exercise 1.29:* Simpson's Rule is a more accurate method of
;;   numerical integration than the method illustrated above.  Using
;;   Simpson's Rule, the integral of a function f between a and b is
;;   approximated as

;;        h
;;        - (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n)
;;        3

;;   where h = (b - a)/n, for some even integer n, and y_k = f(a + kh).
;;   (Increasing n increases the accuracy of the approximation.)
;;   Define a procedure that takes as arguments f, a, b, and n and
;;   returns the value of the integral, computed using Simpson's Rule.
;;   Use your procedure to integrate `cube' between 0 and 1 (with n =
;;   100 and n = 1000), and compare the results to those of the
;;   `integral' procedure shown above.

(define (sum term a next b)
  (if (> a b)
      0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x)
  (* x x x))

(integral cube 0 1 0.01)

(define (add-2 n) (+ n 2))

(define (simpsons-rule f a b n)
  (let ((h (/ (- b a) n)))
    (let ((y (lambda (k) (f (+ a (* k h))))))
      (* (+ (y 0)
            (* (sum y (+ a 1) add-2 (- n 1)) 4)
            (* (sum y (+ a 2) add-2 (- n 2)) 2)
            (y n))
         (/ h 3)))))
        
(simpsons-rule cube 0 1 1000)

(integral cube 0 4 0.01)
(simpsons-rule cube 0 4 100)
