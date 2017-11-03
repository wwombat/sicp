;;   *Exercise 1.30:* The `sum' procedure above generates a linear
;;   recursion.  The procedure can be rewritten so that the sum is
;;   performed iteratively.  Show how to do this by filling in the
;;   missing expressions in the following definition:

(define (sum term a next b)
  (if (> a b)
      0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (ident a) a)

(sum ident 0 (lambda (a) (+ a 1)) 4)

(sum-iter ident 0 (lambda (a) (+ a 1)) 4)
