;;   *Exercise 2.17:* Define a procedure `last-pair' that returns the
;;   list that contains only the last element of a given (nonempty)
;;   list:

;;        (last-pair (list 23 72 149 34))
;;        (34)

(define (last-pair a)
  (cond ((null? a) 'nil)
        ((null? (cdr a)) (car a))
        ((= 1 1) (last-pair (cdr a))))) 

(last-pair (list 23 72 149 34))

;;   *Exercise 2.18:* Define a procedure `reverse' that takes a list as
;;   argument and returns a list of the same elements in reverse order:

;;        (reverse (list 1 4 9 16 25))
;;        (25 16 9 4 1)

(define (append list1 list2)
  (if (null? list1)
      list2
    (cons (car list1) (append (cdr list1) list2))))

(define (reverse a)
  (cond ((null? a) 'nil)
        ((null? (cdr a)) (list (car a)))
        ((= 1 1) (append (reverse (cdr a)) (list (car a))))))

(define (reverse-iter a)
  (define (helper a b)
    (cond ((null? a) b)
          ((= 1 1) (helper (cdr a) (cons (car a) b)))))
  (helper (cdr a) (list (car a))))

(reverse-iter (list 1 4 9 16 25))
(reverse (list 1 4 9 16 25))

;;   *Exercise 2.19:* Consider the change-counting program of section
;;   *Note 1-2-2::.  It would be nice to be able to easily change the
;;   currency used by the program, so that we could compute the number
;;   of ways to change a British pound, for example.  As the program is
;;   written, the knowledge of the currency is distributed partly into
;;   the procedure `first-denomination' and partly into the procedure
;;   `count-change' (which knows that there are five kinds of U.S.
;;   coins).  It would be nicer to be able to supply a list of coins to
;;   be used for making change.

;;   We want to rewrite the procedure `cc' so that its second argument
;;   is a list of the values of the coins to use rather than an integer
;;   specifying which coins to use.  We could then have lists that
;;   defined each kind of currency:

        (define us-coins (list 50 25 10 5 1))

        (define uk-coins (list 100 50 20 10 5 2 1 0.5))

;;   We could then call `cc' as follows:

;;        (cc 100 us-coins)
;;        292

;;   To do this will require changing the program `cc' somewhat.  It
;;   will still have the same form, but it will access its second
;;   argument differently, as follows:

        (define (cc amount coin-values)
          (cond ((= amount 0) 1)
                ((or (< amount 0) (no-more? coin-values)) 0)
                (else
                 (+ (cc amount
                        (except-first-denomination coin-values))
                    (cc (- amount
                           (first-denomination coin-values))
                        coin-values)))))

(define (first-denomination coins)
  (car coins)) 

(define (except-first-denomination coins)
  (cdr coins)) 

(define (no-more? coins)
  (null? coins)) 

(define us-coins (list 1 10 50 5 25))

(cc 100 us-coins)

;;   Define the procedures `first-denomination',
;;   `except-first-denomination', and `no-more?' in terms of primitive
;;   operations on list structures.  Does the order of the list
;;   `coin-values' affect the answer produced by `cc'?  Why or why not?

;; It is not affected by the order of the list, because, regardless of
;; the ~exact~ order of the search, it is still exhaustive!
;; Note that the key truth behind the approach still hold:
;; For any combination of remaining value and remaining possible coin
;; values, any possible remaining ways to reduce the value to zero
;; either make use of the first remaining possible coin value, and
;; hence is in the
;;    (cc (- amount (first-denomination coin-values)) coin-values)
;; part of the coin universe OR
;; it does NOT make use of that value, in which case it must be
;; in the
;;    (cc amount (except-first-deonomination coin-values))
;; part of the coin universe.

