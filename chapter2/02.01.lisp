
     (define (add-rat x y)
       (make-rat (+ (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))

     (define (sub-rat x y)
       (make-rat (- (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))

     (define (mul-rat x y)
       (make-rat (* (numer x) (numer y))
                 (* (denom x) (denom y))))

     (define (div-rat x y)
       (make-rat (* (numer x) (denom y))
                 (* (denom x) (numer y))))

     (define (equal-rat? x y)
       (= (* (numer x) (denom y))
          (* (numer y) (denom x))))

     (define (print-rat x)
       (newline)
       (display (numer x))
       (display "/")
       (display (denom x)))

     (define (numer x) (car x))

     (define (denom x) (cdr x))

     (define (make-rat n d)
       (let ((g (gcd n d)))
         (cons (/ n g) (/ d g))))

     (define one-third (make-rat 1 3))

     (print-rat (add-rat one-third one-third))

;;   *Exercise 2.1:* Define a better version of `make-rat' that handles
;;   both positive and negative arguments.  `Make-rat' should normalize
;;   the sign so that if the rational number is positive, both the
;;   numerator and denominator are positive, and if the rational number
;;   is negative, only the numerator is negative.

;; try one

(define (make-rat n d)
  (let ((cons-helper (lambda (n d)
                       (define g (gcd n d))
                       (cons (/ n g) (/ d g)))))
    (if (> (* n d) 0)
        (cons-helper (abs n) (abs d))
      (cons-helper (- (abs n)) (abs d)))))

;;   -  +  1 mult, 1 comp, 2 abs, 1 -
;;   +  -  1 mult, 1 comp, 2 abs, 1 -
;;   -  -  1 mult, 1 comp, 2 abs
;;   +  +  1 mult, 1 comp, 2 abs

;; hmmm!!! this feels excessive.
;; four cases:
;;
;;   -  +   ->  -  +  0
;;   +  -   ->  -  +  2  |____ aha
;;   -  -   ->  +  +  2  |
;;   +  +   ->  +  +  0

;; try two

(define (make-rat2 n d)
  (let ((cons-helper (lambda (n d)
                       (define g (gcd n d))
                       (cons (/ n g) (/ d g)))))
    (if (> d 0)
        (cons-helper n d)
      (cons-helper (- n) (- d)))))

(make-rat 3 -4) 

(make-rat2 3 -4) 

;;   -  +  1 comp
;;   +  -  1 comp, 2 -
;;   -  -  1 comp, 2 -
;;   +  +  1 comp
