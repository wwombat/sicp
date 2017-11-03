(define (accumulate term a next b combiner null-value)
  (if (> a b)
      null-value
    (combiner (term a)
       (accumulate term (next a) next b combiner null-value))))

(define (product term a next b)
;;   *Exercise 1.33:* You can obtain an even more general version of
;;   `accumulate' (*Note Exercise 1-32::) by introducing the notion of
;;   a "filter" on the terms to be combined.  That is, combine only
;;   those terms derived from values in the range that satisfy a
;;   specified condition.  The resulting `filtered-accumulate'
;;   abstraction takes the same arguments as accumulate, together with
;;   an additional predicate of one argument that specifies the filter.
;;   Write `filtered-accumulate' as a procedure.  Show how to express
;;   the following using `filtered-accumulate':


(define (filtered-accumulate term a next b combiner null-value filter)
  (if (> a b)
      null-value
    (combiner (if (filter a) (term a) null-value)
       (filtered-accumulate term (next a) next b combiner null-value filter))))


;;     a. the sum of the squares of the prime numbers in the interval a
;;        to b (assuming that you have a `prime?' predicate already
;;        written)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (incr a) (+ a 1))

(define (sum-squares-of-primes a b)
  (filtered-accumulate square a incr b (lambda (x y) (+ x y)) 0 prime?))  

(sum-squares-of-primes 0 6) 

;;     b. the product of all the positive integers less than n that are
;;        relatively prime to n (i.e., all positive integers i < n such
;;        that GCD(i,n) = 1).

(define (ident a) a)

(define (sum-rel-prime n)
  (define (relatively-prime? a)
    (= (GCD a n) 1))
  (filtered-accumulate ident
                       0
                       incr
                       (- n 1)
                       (lambda (x y) (+ x y))
                       0
                       relatively-prime?))  

(sum-rel-prime 6)  
