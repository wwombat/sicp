
;;   *Exercise 1.35:* Show that the golden ratio [phi] (section *Note
;;   1-2-2::) is a fixed point of the transformation x |-> 1 + 1/x, and
;;   use this fact to compute [phi] by means of the `fixed-point'
;;   procedure.


     (define tolerance 0.00001)

     (define (fixed-point f first-guess)
       (define (close-enough? v1 v2)
         (< (abs (- v1 v2)) tolerance))
       (define (try guess)
         (let ((next (f guess)))
           (newline)
           (display (exact->inexact guess))
           (if (close-enough? guess next)
               next
               (try next))))
       (try first-guess))

;; Hmm... I feel somewhat foggy-brained today and am at a loss of
;; precisely where to start... So let's do the classic failsafe:
;; let us do a little baby trace
;; 
;; 1.   x
;; 2.   1 + 1/x
;; 3.   1 + 1/(1 + 1/x)
;; 4.   1 + 1/(1 + 1/(1 + 1/x))

;; Hmm... What in the world is PHI again?

;; (a + b)/a = a/b
;; PHI^2 = PHI + 1 = (1 + sqrt(5)) / 2

;; Or let's just check that it's right...

(define (fib-proc x)
  (+ 1 (/ 1 x))) 

 (exact->inexact (fixed-point fib-proc 0.001)) 

;; Hmm, yes. 

;; let's simplify the trace above, simplifying fractions-style
;; 1.    x                                 
;; 2.    1 + 1/x
;;       (x + 1)/x
;; 3.    1 + 1 / ((x + 1)/x) 
;;       1 + x / (x + 1) 
;;       (2x + 1) / (x + 1)
;; 4.    1 + 1 / ((2x + 1) / (x + 1))
;;       1 + (x + 1) / (2x + 1)
;;       ((2x + 1) + (x + 1)) / (2x + 1)
;;       (3x + 2) / (2x + 1)

;; OR

;; 1.    x                                 =   a
;; 2.    1 + 1/x
;;       (x + 1)/x                         =   b / a
;; 3.    1 + 1 / ((x + 1)/x)         
;;       1 + x / (x + 1)            
;;       ((x + 1) + x) / (x + 1)           =  (b + a) / b 
;;       (2x + 1) / (x + 1)                =    c     / b
;; 4.    1 + 1 / ((2x + 1) / (x + 1))
;;       1 + (x + 1) / (2x + 1) 
;;       ((2x + 1) + (x + 1)) / (2x + 1)   =  (c + b) / c
;;       (3x + 2) / (2x + 1)               =    d     / c

;; The pattern becomes clear. Now why does this converge towards
;; Phi??? It's certainly rather fibonnaci like, which is a good
;; sign...

;; Come to think about it, it's very much fibonnaci like.
;; In fact, let's check something...


;; let y   = (a + bx) / (c + ax)
;; let b   = (a + c)
;;
;; f(y)    = 1 + (c + ax)/(a + bx)
;;         = (c + ax + a + bx)/(a + bx)
;;         = ((a + c) + (a + b)x) / (a + bx)
;;         =   b + fx / a + bx

;; f(f(y)) = ((a + b) + (b + f)x) / (b + fx)
;;         =   f + zx / b + fx

;;     y   = (a + bx) / (c + ax)
;;   f(y)  = (b + fx) / (a + bx)
;; f(f(y)) = (f + zx) / (b + fx)

;; let's examine the terms of f(f(y))
;;
;;       z = b + f
;;       b = a + c
;;       f = a + b

;; using the initial values of
;; a = 1
;; b = 1
;; c = 0
;; and substituting into y above,
;; we get (1 + 1x) / (0 + 1x),
;; which reduces to (1 + x) / x, AKA 1 + 1/x

;; f(x)          = 1 + 1/x
;;               = (1 + x) / x

;; Now, let's apply that function once more...

;; f(f(x))       = 1 + (x / (1 + x))
;;               = (2 + 3x) / (1 + 2x)

;; we note the beginning of the fibonnaci sequence:
;; fib(1) = 1
;; fib(2) = 1
;; fib(3) = 2
;; fib(4) = 3

;; by the definition of the fibonacci sequence, we can establish that,
;; for n > 2, if we apply f iteratively n times, starting from a constant k,
;; the result can be expressed as
;;
;; (fib(n - 1) + fib(n)x ) / (fib(n-2) + fib(n-1)x)

;; It is known that, for large n, fib(n) approaches PHI^n/sqrt(5)

;; Taking the limit of the above equation as n approaches positive infinity,
;; we get

;;   ((1/sqrt(5))(PHI^(n - 1) + PHI^(n)x)) / ((1/sqrt(5))/(PHI^(n - 2) + PHI^(n-1)x))
;; = ((PHI^(n - 1) + PHI^(n)x)) / ((PHI^(n - 2) + PHI^(n-1)x))
;; Factor out a PHI^(n-2), and we get...
;; = (PHI/x + PHI^2)/(1/x + PHI)
;; = PHI(1/x + PHI)/(1/x + PHI)
;; = PHI, for large n

;; ^_^ ^_^ ^_^ ^_^
;;
;; :3 yay yay!
;; I feel so smart

;; Thanks to :>, who helped me remember how to factor out stuff
;; so nice :3
