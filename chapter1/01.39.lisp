
 ;;  *Exercise 1.39:* A continued fraction representation of the
 ;;  tangent function was published in 1770 by the German mathematician
 ;;  J.H. Lambert:

 ;;                     x
 ;;       tan x = ---------------
 ;;                       x^2
 ;;               1 - -----------
 ;;                         x^2
 ;;                   3 - -------
 ;;                       5 - ...

 ;;  where x is in radians.  Define a procedure `(tan-cf x k)' that
 ;;  computes an approximation to the tangent function based on
 ;;  Lambert's formula.  `K' specifies the number of terms to compute,
 ;;  as in *Note Exercise 1-37::.

(define try (lambda (n d k i)
              (if (= i k)
                  (/ (n i) (d i))
                (/ (n i) (+ (d i) (try n d k (+ 1 i)))))))

(define (cont-frac n d k)
    (try n d k 0))

(define (tan-cf x k)
  (cont-frac (lambda (a) (+ (if (= a 0) x (- (expt x 2)))))
             (lambda (a) (+ (* 2 a) 1.0))
             k)) 

(tan-cf 4 100) 

(tan 4) 
