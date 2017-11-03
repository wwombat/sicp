
;;   *Exercise 1.38:* In 1737, the Swiss mathematician Leonhard Euler
;;   published a memoir `De Fractionibus Continuis', which included a
;;   continued fraction expansion for e - 2, where e is the base of the
;;   natural logarithms.  In this fraction, the n_i are all 1, and the
;;   D_i are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....  Write
;;   a program that uses your `cont-frac' procedure from *Note Exercise
;;   1-37:: to approximate e, based on Euler's expansion.

(define try (lambda (n d k i)
              (if (= i k)
                  (/ (n i) (d i))
                (/ (n i) (+ (d i) (try n d k (+ 1 i)))))))

(define (cont-frac n d k)
    (try n d k 0))

;; Observe the perodicity of three
;; YES THAT IS RIGHT
;; modular arithmetic returns

(define (e precision)
  (+ (cont-frac (lambda (i) 1)
                (lambda (i)
                  (if (= (remainder i 3) 1)
                      (expt 2 (+ (/ (- i 1) 3.0) 1))
                    1))
                precision)
     2.0)) 

(e 100) 
