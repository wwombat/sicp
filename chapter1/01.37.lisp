
;;   *Exercise 1.37:*
;;     a. An infinite "continued fraction" is an expression of the form

;;                        N_1
;;             f = ---------------------
;;                            N_2
;;                 D_1 + ---------------
;;                                N_3
;;                       D_2 + ---------
;;                             D_3 + ...

;;        As an example, one can show that the infinite continued
;;        fraction expansion with the n_i and the D_i all equal to 1
;;        produces 1/[phi], where [phi] is the golden ratio (described
;;        in section *Note 1-2-2::).  One way to approximate an
;;        infinite continued fraction is to truncate the expansion
;;        after a given number of terms.  Such a truncation--a
;;        so-called finite continued fraction "k-term finite continued
;;        fraction"--has the form

;;                    N_1
;;             -----------------
;;                       N_2
;;             D_1 + -----------
;;                   ...    N_K
;;                       + -----
;;                          D_K

;;        Suppose that `n' and `d' are procedures of one argument (the
;;        term index i) that return the n_i and D_i of the terms of the
;;        continued fraction.  Define a procedure `cont-frac' such that
;;        evaluating `(cont-frac n d k)' computes the value of the
;;        k-term finite continued fraction.  Check your procedure by
;;        approximating 1/[phi] using

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)

;;

(define try (lambda (n d k i)
              (if (= i k)
                  (/ (n i) (d i))
                (/ (n i) (+ (d i) (try n d k (+ 1 i)))))))

(define (cont-frac n d k)
    (try n d k 0))

(define phi*10000 6180)

(define (close-enough? x) (= (truncate (* 10000 x)) phi*10000))

(define (find-close-enough i)
  (let ((close-try (lambda (x) (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) x))))
    (if (close-enough? (close-try i))
        i
      (find-close-enough (+ i 1))))) 

(find-close-enough 0) 

;; ~~ 10 ~~ - let's confirm

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8)

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 9)

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)

;; yup.
  
;;        for successive values of `k'.  How large must you make `k' in
;;        order to get an approximation that is accurate to 4 decimal
;;        places?

;;     b. If your `cont-frac' procedure generates a recursive process,
;;        write one that generates an iterative process.  If it
;;        generates an iterative process, write one that generates a
;;        recursive process.

(define try-iter (lambda (n d k sum)
                   (if (= 0 k)
                       sum
                     (try-iter n d (- k 1) (/ (n k) (+ (d k) sum))))))

(define (cont-frac-iter n d k)
    (try-iter n d k 0))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100)

