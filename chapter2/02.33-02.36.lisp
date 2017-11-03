;;   *Exercise 2.33:* Fill in the missing expressions to complete the
;;   following definitions of some basic list-manipulation operations
;;   as accumulations:

;;        (define (map p sequence)
;;          (accumulate (lambda (x y) <??>) nil sequence))

;;        (define (append seq1 seq2)
;;          (accumulate cons <??> <??>))

;;        (define (length sequence)
;;          (accumulate <??> 0 sequence))

(define nil '()) 
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(map square (list 1 2 3 4)) 
(acc-map square (list 1 2 3 4)) 

(define (acc-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(append '(1 2) '(3 4)) 
(acc-append '(1 2) '(3 4)) 

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(acc-length '(1 2 3 4 5)) 

(define (acc-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;;   *Exercise 2.34:* Evaluating a polynomial in x at a given value of
;;   x can be formulated as an accumulation.  We evaluate the polynomial

;;        a_n r^n | a_(n-1) r^(n-1) + ... + a_1 r + a_0

;;   using a well-known algorithm called "Horner's rule", which
;;   structures the computation as

;;        (... (a_n r + a_(n-1)) r + ... + a_1) r + a_0

;;   In other words, we start with a_n, multiply by x, add a_(n-1),
;;   multiply by x, and so on, until we reach a_0.(3)

;;   Fill in the following template to produce a procedure that
;;   evaluates a polynomial using Horner's rule.  Assume that the
;;   coefficients of the polynomial are arranged in a sequence, from
;;   a_0 through a_n.

;;        (define (horner-eval x coefficient-sequence)
;;          (accumulate (lambda (this-coeff higher-terms) <??>)
;;                      0
;;                      coefficient-sequence))

;;   For example, to compute 1 + 3x + 5x^3 + x^(5) at x = 2 you would
;;   evaluate

;;        (horner-eval 2 (list 1 3 0 5 0 1))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(let ((x 3.4)) (+ 1 (* 3 x) (* 5 (expt x 3)) (expt x 5)))

(horner-eval 3.4 (list 1 3 0 5 0 1))

;;   *Exercise 2.35:* Redefine `count-leaves' from section *Note
;;   2-2-2:: as an accumulation:

(define my-tree (list (list 1 2) (list (list 3 4) 5)))) 

(define (count-leaves t)
  (accumulate (lambda (curr rest)
                (if (pair? curr)
                    (+ (count-leaves curr) rest)
                  (+ 1 rest)))
              0
              t)) 

(define (cl t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (cl (car t))
                 (cl (cdr t))))))

(cl my-tree) 

(count-leaves my-tree) 

;;   *Exercise 2.36:* The procedure `accumulate-n' is similar to
;;   `accumulate' except that it takes as its third argument a sequence
;;   of sequences, which are all assumed to have the same number of
;;   elements.  It applies the designated accumulation procedure to
;;   combine all the first elements of the sequences, all the second
;;   elements of the sequences, and so on, and returns a sequence of
;;   the results.  For instance, if `s' is a sequence containing four
;;   sequences, `((1 2 3) (4 5 6) (7 8 9) (10 11 12)),' then the value
;;   of `(accumulate-n + 0 s)' should be the sequence `(22 26 30)'.
;;   Fill in the missing expressions in the following definition of
;;   `accumulate-n':

;;        (define (accumulate-n op init seqs)
;;          (if (null? (car seqs))
;;              nil
;;              (cons (accumulate op init <??>)
;;                    (accumulate-n op init <??>))))

;; The key here is to realize that you can use map in this way!
;; If we conceptualize the answer we want as the cons of
;; the result of accumulating the car of each item with the results
;; of repeating this procedure on the cdr of each item, then
;; the way to structure this becomes pretty clear!

;; (For fun, consider how you might go about doing this in your
;; favorite imperative paradigm... I imagine the code is fairly
;; different!)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) 

(accumulate-n + 0 s)

;; (22 26 30)
