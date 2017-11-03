;;   *Exercise 2.73:* Section *Note 2-3-2:: described a program that
;;   performs symbolic differentiation:

          (define (deriv exp var)
            (cond ((number? exp) 0)
                  ((variable? exp) (if (same-variable? exp var) 1 0))
                  ((sum? exp)
                   (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var)))
                  ((product? exp)
                   (make-sum
                     (make-product (multiplier exp)
                                   (deriv (multiplicand exp) var))
                     (make-product (deriv (multiplier exp) var)
                                   (multiplicand exp))))
;;                  <MORE RULES CAN BE ADDED HERE>
                  (else (error "unknown expression type -- DERIV" exp))))

;;   We can regard this program as performing a dispatch on the type of
;;   the expression to be differentiated.  In this situation the "type
;;   tag" of the datum is the algebraic operator symbol (such as `+')
;;   and the operation being performed is `deriv'.  We can transform
;;   this program into data-directed style by rewriting the basic
;;   derivative procedure as

          (define (deriv exp var)
             (cond ((number? exp) 0)
                   ((variable? exp) (if (same-variable? exp var) 1 0))
                   (else ((get 'deriv (operator exp)) (operands exp)
                                                      var))))

          (define (operator exp) (car exp))

          (define (operands exp) (cdr exp))

;;     a. Explain what was done above.  Why can't we assimilate the
;;        predicates `number?' and `same-variable?' into the
;;        data-directed dispatch?

;; Because, at least in the implementation we've used, numbers and
;; variables aren't tagged in the same way that expressions are!
;; In our implementation, the operators '(+ *) act analogously
;; to the 'Polar or 'Rectangular tags above. We could re-implement
;; our derivative calculator to expect arguments resembling
;; '(+ (num 3) (var x)), but that approach does not seem like the
;; absolutely prettiest to me.

;;     b. Write the procedures for derivatives of sums and products,
;;        and the auxiliary code required to install them in the table
;;        used by the program above.

    (define (sum-deriv exp)
    (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var)))

    (put 'deriv '+ sum-deriv) 

    (define (prod-deriv exp)
    (make-sum
    (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
    (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))

    (put 'deriv '* prod-deriv) 

;;     c. Choose any additional differentiation rule that you like,
;;        such as the one for exponents (*Note Exercise 2-56::), and
;;        install it in this data-directed system.

;;     d. In this simple algebraic manipulator the type of an
;;        expression is the algebraic operator that binds it together.
;;        Suppose, however, we indexed the procedures in the opposite
;;        way, so that the dispatch line in `deriv' looked like

;;          ((get (operator exp) 'deriv) (operands exp) var)

;;        What corresponding changes to the derivative system are
;;        required?

;; We'd simply have to reverse our arguments to the put procedure,
;; i.e., 

;; (put '+ 'deriv sum-deriv)
;; (put '* 'deriv prod-deriv) 
