;;   *Exercise 2.58:* Suppose we want to modify the differentiation
;;   program so that it works with ordinary mathematical notation, in
;;   which `+' and `*' are infix rather than prefix operators.  Since
;;   the differentiation program is defined in terms of abstract data,
;;   we can modify it to work with different representations of
;;   expressions solely by changing the predicates, selectors, and
;;   constructors that define the representation of the algebraic
;;   expressions on which the differentiator is to operate.

;;     a. Show how to do this in order to differentiate algebraic
;;        expressions presented in infix form, such as `(x + (3 * (x +
;;        (y + 2))))'.  To simplify the task, assume that `+' and `*'
;;        always take two arguments and that expressions are fully
;;        parenthesized.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(deriv '((x * y) * (x + 3)) 'x) 


;;     b. The problem becomes substantially harder if we allow standard
;;        algebraic notation, such as `(x + 3 * (x + y + 2))', which
;;        drops unnecessary parentheses and assumes that multiplication
;;        is done before addition.  Can you design appropriate
;;        predicates, selectors, and constructors for this notation
;;        such that our derivative program still works?

(define (augend s)
  (cond ((null? (cdddr s)) ;; the third argument
         (caddr s))
        (else (cddr s))))

(augend '(x + 3 * (x + y + 2))) 

(define (multiplicand s)
  (cond ((null? (cdddr s)) ;; the third argument
         (caddr s))
        (else (cddr s))))

(define (get-head s)
  (car s)) 

(get-head '(2 + 3)) 

(define (get-tail s)
  (cond ((null? (cdddr s)) (caddr s))
        (else (cddr s)))) 

(define (op<? x y)
  (define (value op)
    (cond ((eq? op '+) 1)
          ((eq? op '*) 2)
          (else 0)))
  (< (value x) (value y))) 

(multiplicand '(2 * 3 + 2)) 

(deriv '(x + 3 * (x + y + 2)) 'x) 

(define (get-op s)
  (if (null? (cdr s))
      '()
    (cadr s)))

(get-op '(x * 3)) 

(define (trim s)
  (cond ((null? s) '())
        ((null? (cdr s)) '())
        (else (cons (car s) (trim (cdr s))))))

(define (parens-get-head s)
  (define (get-longest-helper s op)
    (cond ((null? s) '())
          ((op<? op (get-op s)) (list (car s)))
          (else (cons (car s) (get-longest-helper (cdr s) op)))))
  (trim (trim (get-longest-helper s (get-op s)))))

(define (parens-get-tail s)
  (define (get-tail-helper s op)
    (cond ((null? s) '())
          ((op<? op (get-op s)) s)
          (else (get-tail-helper (cdr s) op))))
  (get-tail-helper s (get-op s)))

(parens-get-head '(2 + 3 + (3 + 2 * 8) + 8 * 4 + 5))

(parens-get-tail '(2 + 3 + (3 + 2 * 8) + 8 * 4 + 5))

(define (get-tail s)
  (cond ((null? (cdddr s)) ;; the third argument
         (caddr s))
        (else (cddr s))))

(define (all-one-op? s)
  (define (helper s op)
    (cond ((not (pair? s)) #t)
          ((not (eq? (get-op s) op)) #f)
          (else (helper (get-tail s) op))))
  (helper s (get-op s)))

(get-tail '(3 + 2 * 4 * 3)) 

; (let ((s-head (get-head s))
;       (s-tail (get-tail s)))

(all-one-op? 2) 

(get-head '((2 + 3) + 1))

(get-tail '(2 + 1))

(define (parensify s)
  (cond ((not (pair? s)) s)
        ((null? (parens-get-tail s)) s)
        ((all-one-op? s) s)
        (else (list (parensify (parens-get-head s))
                    (get-op s)
                    (parensify (parens-get-tail s))))
        ))

(parensify '(3 + 2 * 1 + 3 * 3 + 2)) 

(parensify '(2 * 1 + 3 * 3 + 2)) 

;; What we want to do here is look ahead 


;; OK! that's all nonsense, try two:

;; One 

;; A better approach is to keep our differentiation code
;; the same and implement a preproccessing step that 
;; parenthesizes our input before feeding it into the
;; differentation algorithm! While this may possible
;; increase our costs, as a naive implementation necess-
;; itates multiple passes through the data, it preserves
;; a logical separation between the two parts out our
;; operation, the first being the application of our
;; arbitrary operator precedence rules, and the second
;; being the differentation rules.

;; Multiple approaches to parenthesization occur, and,
;; as one might expect, it is a fairly well-trod path!
;; Here, I implement a somewhat brute force solution -
;; given a list of operators in order of precedence, we
;; search for the operator with the ~lowest precedence~ 
;; that has not been examined, and then, if we find it,
;; split the expression around it into two units, which
;; we then recursively evaluate using the same algorithm.
;; The base case of this procedure is a single literal,
;; which is returned un-transformed.

;; input:
;; (parensify '(3 + 2 * 1 + 3 * 3 + 2)) 

;; desired output:
;; (3 + (2 * 1) + (3 * 3) + 2)

;; operator table
(define operators '(+ *)) 

(define (unwrap-if-alone exp)
  (if (null? (cdr exp)) (car exp) exp)) 

;; split on an operator, or return nil
;; if the operator is not present
(define (split s op)
  (define (helper left right op)
    (cond ((null? right) '())
          ((eq? (car right) op) (list (unwrap-if-alone left)
                                      (car right)
                                      (unwrap-if-alone (cdr right))))
          (else (helper (append left (list (car right))) (cdr right) op))))
  (helper '() s op))

(split '(10 * 3 + 1 + 2 * 3) '+)
;; desired output:
;; ((10 * 3) + (1 + 2 * 3))

(split '(1 + 1) '+) 
;; desired output:
;; (1 + 1)

(split '(1 + 1) '*)
;; desired output:
;; '()

;; Now that we have our components ready, we can implement
;; the actual algorithm!

(define (split-on-first-op s op-list)
  (if (null? op-list) '()
    (let ((result (split s (car op-list))))
      (if (not (null? result)) result
        (split-on-first-op s (cdr op-list))))))

(split-on-first-op '(10 * 3 * 3) operators) 
;; desired output:
;; (10 * (3 * 3))

(split-on-first-op '(10 * 3 + 1 + 2 * 3) operators)
;; desired output:
;; ((10 * 3) + (1 + 2 * 3))

(define (left exp)
  (car exp)) 

(define (op exp)
  (cadr exp))

(define (right exp)
  (caddr exp)) 

(define (parenthesize s)
  (if (not (pair? s)) s
    (let ((result (split-on-first-op s operators)))
      (list (parenthesize (left result))
            (op result)
            (parenthesize (right result)))))) 

(define (deriv-2 s var)
  (deriv (parenthesize s) var)) 

(deriv-2 '(x * y * (x + 3)) 'x) 
(deriv '((x * y) * (x + 3)) 'x) 

(deriv-2 '(x + 3 * (x + y + 2)) 'x) 
(deriv '(x + (3 * (x + y + 2))) 'x)

