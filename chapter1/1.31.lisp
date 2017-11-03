;;   *Exercise 1.31:*
;;     a. The `sum' procedure is only the simplest of a vast number of
;;        similar abstractions that can be captured as higher-order
;;        procedures.(3)  Write an analogous procedure called `product'
;;        that returns the product of the values of a function at
;;        points over a given range.  Show how to define `factorial' in
;;        terms of `product'.  Also use `product' to compute
;;        approximations to [pi] using the formula(4)

;;             pi   2 * 4 * 4 * 6 * 6 * 8 ...
;;             -- = -------------------------
;;              4   3 * 3 * 5 * 5 * 7 * 7 ...

;;     b. If your `product' procedure generates a recursive process,
;;        write one that generates an iterative process.  If it
;;        generates an iterative process, write one that generates a
;;        recursive process.


;; a.

(define (product term a next b)
  (if (> a b)
      1
    (* (term a)
       (product term (next a) next b))))

(define (ident a) a)
(define (incr a) (+ a 1))

(define (factorial n)
    (product ident 1 incr n))

(define (pi-ident i)
  (+ i (remainder i 2)))

(define (pi n)
  (* 4 (/
        (product pi-ident 2 incr (+ 1 n))
        (product (lambda (a) (+ 1 (pi-ident a))) 1 incr n))))

(exact->inexact (pi 3000))

(exact->inexact (pi 19))

;; b.

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
      (iter (next a) (* result (term a)))))
  (iter a 1)) 

(product pi-ident 2 incr 6) 

(product-iter pi-ident 2 incr 6) 
