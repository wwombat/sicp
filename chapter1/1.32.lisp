;;   *Exercise 1.32:*
;;     a. Show that `sum' and `product' (*Note Exercise 1-31::) are
;;        both special cases of a still more general notion called
;;        `accumulate' that combines a collection of terms, using some
;;        general accumulation function:

;;             (accumulate combiner null-value term a next b)

;;        `Accumulate' takes as arguments the same term and range
;;        specifications as `sum' and `product', together with a
;;        `combiner' procedure (of two arguments) that specifies how
;;        the current term is to be combined with the accumulation of
;;        the preceding terms and a `null-value' that specifies what
;;        base value to use when the terms run out.  Write `accumulate'
;;        and show how `sum' and `product' can both be defined as
;;        simple calls to `accumulate'.

;;     b. If your `accumulate' procedure generates a recursive process,
;;        write one that generates an iterative process.  If it
;;        generates an iterative process, write one that generates a
;;        recursive process.

;; a.

(define (accumulate term a next b combiner null-value)
  (if (> a b)
      null-value
    (combiner (term a)
       (accumulate term (next a) next b combiner null-value))))

(define (product term a next b)
  (accumulate term a next b (lambda (x y) (* x y)) 1))

(define (sum term a next b)
  (accumulate term a next b (lambda (x y) (+ x y)) 0))

(define (ident a) a)
(define (incr a) (+ a 1))

(sum ident 0 incr 4)
(product ident 1 incr 4)

;; b

(define (accumulate-iter term a next b combiner null-value)
  (define (iter a result)
    (if (> a b)
        result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-iter term a next b)
  (accumulate-iter term a next b (lambda (x y) (+ x y)) 0))

(define (product-iter term a next b)
  (accumulate-iter term a next b (lambda (x y) (* x y)) 0))

(sum ident 0 incr 4)

(sum-iter ident 0 incr 4)

(product ident 1 incr 4)

(product-iter ident 1 incr 4)
