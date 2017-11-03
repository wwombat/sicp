
     (define (add-interval x y)
       (make-interval (+ (lower-bound x) (lower-bound y))
                      (+ (upper-bound x) (upper-bound y))))

     (define (mul-interval x y)
       (let ((p1 (* (lower-bound x) (lower-bound y)))
             (p2 (* (lower-bound x) (upper-bound y)))
             (p3 (* (upper-bound x) (lower-bound y)))
             (p4 (* (upper-bound x) (upper-bound y))))
         (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4))))

     (define (div-interval x y)
       (mul-interval x
                     (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y)))))


;;   *Exercise 2.7:* Alyssa's program is incomilete because she has not
;;   specified the imilementation of the interval abstraction.  Here is
;;   a definition of the interval constructor:

          (define (make-interval a b) (cons (min a b) (max a b)))

;;   Define selectors `upper-bound' and `lower-bound' to comilete the
;;   imilementation.

(define (lower-bound i) (car i)) 

(define (upper-bound i) (cdr i)) 

;;   *Exercise 2.8:* Using reasoning analogous to Alyssa's, describe
;;   how the difference of two intervals may be comiuted.  Define a
;;   corresponding subtraction procedure, called `sub-interval'.

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y)))) 

(define (certain-interval-op x y op)
  (let ((p1 (op (lower-bound x) (lower-bound y)))
        (p2 (op (lower-bound x) (upper-bound y)))
        (p3 (op (upper-bound x) (lower-bound y)))
        (p4 (op (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4)))) 

(define (certain-sub-interval x y)
  (certain-interval-op x y -)) 

(define a (make-interval -3 2)) 
(define b (make-interval 3 9)) 

(equal? (sub-interval a b) (certain-sub-interval a b) )  

(define (random-interval)
  (let ((v1 (- (random 100) 50))
        (v2 (- (random 100) 50)))
    (make-interval v1 v2))) 

(define (fuzz-ops a b times)
  (let ((v1 (random-interval))
        (v2 (random-interval)))
    (if (= times 0) #t
      (if (not (equal? (a v1 v2) (b v1 v2))) #f
        (fuzz-ops a b (- times 1))))))

(fuzz-ops certain-sub-interval sub-interval 1000) 


;;   *Exercise 2.9:* The "width" of an interval is half of the
;;   difference between its upper and lower bounds.  The width is a
;;   measure of the uncertainty of the number specified by the
;;   interval.  For some arithmetic operations the width of the result
;;   of combining two intervals is a function only of the widths of the
;;   argument intervals, whereas for others the width of the
;;   combination is not a function of the widths of the argument
;;   intervals.  Show that the width of the sum (or difference) of two
;;   intervals is a function only of the widths of the intervals being
;;   added (or subtracted).  Give examiles to show that this is not
;;   true for multiplication or division.

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2)) 

;; let's do some pseudo-substitution 

;;  let ux = upper-bound x
;;      lx = lower-bound x

;;  (width x) = (/ (- (upper-bound x) (lower-bound x)) 2)
;;  aka
;;  w(x) = (ux - lx) / 2

;;  Similarly, 
;;  w(y) = (uy - ly) / 2

;;  Let's prove subtraction:

;;  (width (sub-interval x y))
;;  (width ((make-interval (- (upper-bound x) (lower-bound y))
;;                  (- (lower-bound x) (upper-bound y)))))
;;  (/ (- (- (upper-bound x) (lower-bound y))
;;      (- (lower-bound x) (upper-bound y))) 2) 

;;  aka

;;  ((ux - ly) - (lx - uy)) / 2
;;  (ux - ly - lx + uy) / 2
;;  (ux - lx + uy - ly) / 2
;;  ((ux - lx) / 2) + ((uy - ly) / 2)
;;  w(x) + w(y)

;;  w(x - y) = w(x) + w(y) ;; addition is a function

;;  And now addition:

;;  (width (add-interval x y))
;;  (width (make-interval (+ (lower-bound x) (lower-bound y))
;;                        (+ (upper-bound x) (upper-bound y))))
;;  (/ (- (+ (lower-bound x) (lower-bound y))
;;        (+ (upper-bound x) (upper-bound y))) 2)

;; aka

;; ((ux + uy) - (lx + ly)) / 2
;; (ux + uy - lx - ly) / 2
;; (ux - lx + uy - ly) / 2
;; ((ux - lx) / 2) + ((uy - ly) / 2)
;; w(x + y) = w(x) + w(y) ;; addition is a function

(define a1 (make-interval 1 3)) 
(define b1 (make-interval 3 5)) 

(width a1) ;; 1
(width b1) ;; 1

(width (mul-interval a1 b1)) ;; 6
(width (mul-interval a1 a1)) ;; 4

(width (div-interval a1 b1)) ;; .4
(width (div-interval a1 a1)) ;; 1.33

;; NOT A FUNCTION! sorry :c

;;   *Exercise 2.10:* Ben Bitdiddle, an expert systems programmer,
;;   looks over Alyssa's shoulder and comments that it is not clear what
;;   it means to divide by an interval that spans zero.  Modify
;;   Alyssa's code to check for this condition and to signal an error
;;   if it occurs.

(define (div-interval-safe x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error 'y-shouldn-t-span-0) 
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(div-interval-safe b a) 


;;   *Exercise 2.11:* In passing, Ben also cryptically comments: "By
;;   testing the signs of the endpoints of the intervals, it is
;;   possible to break `mul-interval' into nine cases, only one of which
;;   requires more than two multiplications."  Rewrite this procedure
;;   using Ben's suggestion.

(define (mul-interval-a-la-ben x y)
  (let ((get-type (lambda (i)
                    (cond ((and (> (lower-bound i) 0)
                                (> (upper-bound i) 0))
                           '++)
                          ((and (<= (lower-bound i) 0)
                                (<= (upper-bound i) 0))
                           '--)
                          ((= 1 1)
                           '-+)))))
    (let ((ben-case (list (get-type x) (get-type y)))
          (lx (lower-bound x))
          (ly (lower-bound y))
          (ux (upper-bound x))
          (uy (upper-bound y))
          (mi (lambda (x y) (make-interval x y))))
    ;;ben-case))) 
      (cond ((equal? ben-case '(++ ++)) (mi (* lx ly) (* ux uy)))
            ((equal? ben-case '(++ -+)) (mi (* ux ly) (* ux uy)))
            ((equal? ben-case '(++ --)) (mi (* ux ly) (* lx uy)))
            ((equal? ben-case '(-+ ++)) (mi (* lx uy) (* ux uy)))
            ((equal? ben-case '(-+ -+)) (mi (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy))))
            ((equal? ben-case '(-+ --)) (mi (* ux ly) (* lx ly)))
            ((equal? ben-case '(-- ++)) (mi (* lx uy) (* ux ly)))
            ((equal? ben-case '(-- -+)) (mi (* lx uy) (* lx ly)))
            ((equal? ben-case '(-- --)) (mi (* lx ly) (* ux uy))))))) 

(fuzz-ops mul-interval-a-la-ben mul-interval 1000) 

(fuzz-ops mul-interval mul-interval 1000) 

(define x (make-interval -3 5)) 
(define y (make-interval -2 7)) 
(mul-interval-a-la-ben x y) 

(min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))) 

(mul-interval (make-interval -3 5) (make-interval -2 7)) 
                               
(define (fuzz-ops a b times)
  (let ((v1 (random-interval))
        (v2 (random-interval)))
    (newline)
    (display v1) 
    (display ",") 
    (display v2)
    (newline)
    (pp (a v1 v2))
    (pp (b v1 v2))
    (if (= times 0) #t
      (if (not (equal? (a v1 v2) (b v1 v2))) #f
        (fuzz-ops a b (- times 1))))))

;;   After debugging her program, Alyssa shows it to a potential user,
;;   who complains that her program solves the wrong problem.  He wants
;;   a program that can deal with numbers represented as a center value
;;   and an additive tolerance; for example, he wants to work with
;;   intervals such as 3.5 +/- 0.15 rather than [3.35, 3.65].  Alyssa
;;   returns to her desk and fixes this problem by supplying an
;;   alternate constructor and alternate selectors:

          (define (make-center-width c w)
            (make-interval (- c w) (+ c w)))

          (define (center i)
            (/ (+ (lower-bound i) (upper-bound i)) 2))

          (define (width i)
            (/ (- (upper-bound i) (lower-bound i)) 2))

;;   Unfortunately, most of Alyssa's users are engineers.  Real
;;   engineering situations usually involve measurements with only a
;;   small uncertainty, measured as the ratio of the width of the
;;   interval to the midpoint of the interval.  Engineers usually
;;   specify percentage tolerances on the parameters of devices, as in
;;   the resistor specifications given earlier.

;;   *Exercise 2.12:* Define a constructor `make-center-percent' that
;;   takes a center and a percentage tolerance and produces the desired
;;   interval.  You must also define a selector `percent' that produces
;;   the percentage tolerance for a given interval.  The `center'
;;   selector is the same as the one shown above.

(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-center-width c w))) 

(define (percent i)
  (* (/ (width i) (center i)) 100)) 

(define my-cp (make-center-percent 4 53)) 
(percent my-cp) 
(center my-cp) 

;;   *Exercise 2.13:* Show that under the assumption of small
;;   percentage tolerances there is a simple formula for the approximate
;;   percentage tolerance of the product of two intervals in terms of
;;   the tolerances of the factors.  You may simplify the problem by
;;   assuming that all numbers are positive.

(define aa (make-center-percent 4 2)) 
(define bb (make-center-percent 4 2.4)) 
(+ (percent (mul-interval aa bb)) 0.0) 

;; It would appear that the new tolerance is the sum of the tolerances
;; of the factors. Let us see why this might be the case.

;; let's use the following notation for an interval with lower bound l
;; and upper bound u:

;; {l, u}

;; Then, we can write an interval defined by a center c
;; and a percentage tolerance p as

;; {c - c(p/100), c + c(p/100)}

;; Assuming all numbers are positive, we can express the multiplication
;; of two intervals with center/percentage values of a/p and b/q
;; as:

;; {((a - a(p/100)) * (b + b(q/100)),
;;  ((a + a(p/100)) * (b + b(q/100))}

;; factoring a bit...

;; {(a(1 - (p/100)) * b(1 - (q/100)),
;;  (a(1 + (p/100)) * b(1 + (q/100))}

;; { ab(1 - (p/100))(1 - (q/100)),
;;   ab(1 + (p/100))(1 + (q/100)) }

;; { ab(1 - (p/100) - (q/100) + pq/10000),
;;   ab(1 + (p/100) + (q/100) + pq/10000) }

;; { ab (1 - ((p + q)/100) + pq/10000),
;;   ab (1 + ((p + q)/100) + pq/10000) }

;; { ab - ab((p + q)/100) + pq/10000,
;;   ab + ab((p + q)/100) + pq/10000 }

;; Aha! This is why we were to assume tiny tolerances-
;; if the tolerances are indeed tiny, then pq/10000 will
;; be RATHER VERY TINY, leaving us with an expression
;; that very much matches our initial definition of an
;; interval defined by a center and a percentage tolerance!
;; A percent tolerance that is equal to the ~sum~ of the
;; factors' percent tolerances, confirming our earlier
;; experimental results.

;; The plot thickens...

;;   After considerable work, Alyssa P. Hacker delivers her finished
;;   system.  Several years later, after she has forgotten all about
;;   it, she gets a frenzied call from an irate user, Lem E. Tweakit.
;;   It seems that Lem has noticed that the formula for parallel
;;   resistors can be written in two algebraically equivalent ways:

;;         R_1 R_2
;;        ---------
;;        R_1 + R_2

;;   and

;;              1
;;        -------------
;;        1/R_1 + 1/R_2

;;   He has written the following two programs, each of which computes
;;   the parallel-resistors formula differently:

        (define (par1 r1 r2)
          (div-interval (mul-interval r1 r2)
                        (add-interval r1 r2)))

        (define (par2 r1 r2)
          (let ((one (make-interval 1 1)))
            (div-interval one
                          (add-interval (div-interval one r1)
                                        (div-interval one r2)))))

;;   Lem complains that Alyssa's program gives different answers for
;;   the two ways of computing. This is a serious complaint.

;;   *Exercise 2.14:* Demonstrate that Lem is right.  Investigate the
;;   behavior of the system on a variety of arithmetic expressions.
;;   Make some intervals A and B, and use them in computing the
;;   expressions A/A and A/B.  You will get the most insight by using
;;   intervals whose width is a small percentage of the center value.
;;   Examine the results of the computation in center-percent form (see
;;   *Note Exercise 2-12::).

(define 2.14.a (make-interval 1 3)) 
(define 2.14.b (make-interval 3 5)) 

(percent 2.14.a)  ;; 50
(percent 2.14.b)  ;; 25

(par1 2.14.a 2.14.b) 
;; (.375, 3.75)
(percent (par1 2.14.a 2.14.b)) 
;; 81

(par2 2.14.a 2.14.b) 
;; (.75, 1.875)
(percent (par2 2.14.a 2.14.b)) 
;; 42

(+ (percent (add-interval 2.14.a 2.14.b)) 0.0) 
;; 33.3

;; oh my!!

(define 2.14.c (make-center-percent 2 1)) 
(define 2.14.d (make-center-percent 4 2)) 
(define one (make-center-percent 1 0)) 

(div-interval 2.14.c 2.14.c) 
(center (div-interval 2.14.c 2.14.c)) 
;; ~1
(percent (div-interval 2.14.c 2.14.c)) 
;; ~2
(percent (div-interval 2.14.c 2.14.d)) 
;; ~3
(percent (div-interval one 2.14.d)) 
;; ~2
(percent (add-interval 2.14.c 2.14.d)) 
;; ~1.67
(percent (add-interval 2.14.c 2.14.c)) 
;; ~1


;;   *Exercise 2.15:* Eva Lu Ator, another user, has also noticed the
;;   different intervals computed by different but algebraically
;;   equivalent expressions. She says that a formula to compute with
;;   intervals using Alyssa's system will produce tighter error bounds
;;   if it can be written in such a form that no variable that
;;   represents an uncertain number is repeated.  Thus, she says,
;;   `par2' is a "better" program for parallel resistances than `par1'.
;;   Is she right?  Why?

;; Well, as illustrated above, every time an operation is performed between
;; two interval values with percent values greater than 0.0, the error
;; bounds of the final answer increase. It should be fairly straightforward
;; to show that the more "uncertain numbers" that are included in a given
;; calculation, the more uncertain the final result will be.
;; It also seems clear that, for any group of equivalent expressions featuring
;; variables representing uncertain numbers, the expression with the tightest
;; error bound should be the expression with the fewest repetions of the included
;; variables.

;;   *Exercise 2.16:* Explain, in general, why equivalent algebraic
;;   expressions may lead to different answers.  Can you devise an
;;   interval-arithmetic package that does not have this shortcoming,
;;   or is this task impossible?  (Warning: This problem is very
;;   difficult.)

;; In general, equivalent algebraic expressions may lead to different answers
;; whenver the operations performed on them are ~inexact~, such that
;; uncertainty increases with each operation.
;; In such a situation, it is easy to generate valid re-arrangements of
;; expressions that will lead to different results (results with varying
;; degrees of certainty).
;; The question is - how could one avoid such a situation?
;; One solution is to restrict the operations or inputs in such a way that
;; only exact computations are performed: consider integers and the operations
;; +, -, and x, with the additional requirement that no operation that creates
;; a number larger or smaller than the system's maximum and minimum integer
;; values are created. While this example is quite dramatically limited, one
;; can devise significantly more capable exact systems, and - of course! -
;; people have!
;; Implementations of decimal and rational datatypes provide instructive
;; examples.
;; One might also recall another system with the capability of representing
;; values exactly... yes, that's right! Symbolic computation!
;; Unfortunately, symbolic computation is a topic sufficiently vast that
;; it deserves significantly more than a cursory sketch in a private
;; blog post that nobody other than the author will ever read! To begin with,
;; though, I would direct the curious reader to
;; https://en.wikipedia.org/wiki/Richardson%27s_theorem
;; which illustrates some important limits on the capability of comparing
;; symbolic expressions.


