
;;   *Exercise 1.44:* The idea of "smoothing" a function is an
;;   important concept in signal processing.  If f is a function and dx
;;   is some small number, then the smoothed version of f is the
;;   function whose value at a point x is the average of f(x - dx),
;;   f(x), and f(x + dx).  Write a procedure `smooth' that takes as
;;   input a procedure that computes f and returns a procedure that
;;   computes the smoothed f.  It is sometimes valuable to repeatedly
;;   smooth a function (that is, smooth the smoothed function, and so
;;   on) to obtained the "n-fold smoothed function".  Show how to
;;   generate the n-fold smoothed function of any given function using
;;   `smooth' and `repeated' from *Note Exercise 1-43::.


(define (compose f g)
  (lambda (x) (f (g x)))) 

(define (repeated-iter f n)
  (define (iter f i result)
    (if (= i 1) result
      (iter f (- i 1) (compose f result))))
  (iter f n f)) 

(define (smooth f)
  (define dx 0.1) 
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

((smooth square) 2) 

(sin 20) 

(((repeated smooth 3) sin) 20)

;; I will test this later. haha
;; yes
;; ;-;
