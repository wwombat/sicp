;;==============
;; EVALUATION
;;==============

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
    (let ((left (eval (first-operand exps) env)))
      (let ((right (list-of-values (rest-operands exps) env)))
        (define left (eval (first-operand exps) env))
        (cons left right)))))


(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;==============
;; EXPRESSIONS
;;==============

;; * The only self-evaluating items are numbers and strings:

          (define (self-evaluating? exp)
            (cond ((number? exp) true)
                  ((string? exp) true)
                  (else false)))

;; * Variables are represented by symbols:

          (define (variable? exp) (symbol? exp))

;; * Quotations have the form `(quote <TEXT-OF-QUOTATION>)':(1)

          (define (quoted? exp)
            (tagged-list? exp 'quote))

          (define (text-of-quotation exp) (cadr exp))

;;   `Quoted?' is defined in terms of the procedure `tagged-list?',
;;   which identifies lists beginning with a designated symbol:

          (define (tagged-list? exp tag)
            (if (pair? exp)
                (eq? (car exp) tag)
                false))

;; * Assignments have the form `(set! <VAR> <VALUE>)':

          (define (assignment? exp)
            (tagged-list? exp 'set!))

          (define (assignment-variable exp) (cadr exp))

          (define (assignment-value exp) (caddr exp))

;; * Definitions have the form

;;        (define <VAR> <VALUE>)

;;   or the form

;;        (define (<VAR> <PARAMETER_1> ... <PARAMETER_N>)
;;          <BODY>)

;;   The latter form (standard procedure definition) is syntactic sugar
;;   for

;;        (define <VAR>
;;          (lambda (<PARAMETER_1> ... <PARAMETER_N>)
;;            <BODY>))

;;   The corresponding syntax procedures are the following:

          (define (definition? exp)
            (tagged-list? exp 'define))

          (define (definition-variable exp)
            (if (symbol? (cadr exp))
                (cadr exp)
                (caadr exp)))

          (define (definition-value exp)
            (if (symbol? (cadr exp))
                (caddr exp)
                (make-lambda (cdadr exp)   ; formal parameters
                             (cddr exp)))) ; body

;; * `Lambda' expressions are lists that begin with the symbol `lambda':

          (define (lambda? exp) (tagged-list? exp 'lambda))

          (define (lambda-parameters exp) (cadr exp))

          (define (lambda-body exp) (cddr exp))

;;   We also provide a constructor for `lambda' expressions, which is
;;   used by `definition-value', above:

          (define (make-lambda parameters body)
            (cons 'lambda (cons parameters body)))

;; * Conditionals begin with `if' and have a predicate, a consequent,
;;   and an (optional) alternative.  If the expression has no
;;   alternative part, we provide `false' as the alternative.(2)

          (define (if? exp) (tagged-list? exp 'if))

          (define (if-predicate exp) (cadr exp))

          (define (if-consequent exp) (caddr exp))

          (define (if-alternative exp)
            (if (not (null? (cdddr exp)))
                (cadddr exp)
                'false))

;;   We also provide a constructor for `if' expressions, to be used by
;;   `cond->if' to transform `cond' expressions into `if' expressions:

          (define (make-if predicate consequent alternative)
            (list 'if predicate consequent alternative))

;; * `Begin' packages a sequence of expressions into a single
;;   expression.  We include syntax operations on `begin' expressions
;;   to extract the actual sequence from the `begin' expression, as
;;   well as selectors that return the first expression and the rest of
;;   the expressions in the sequence.(3)

          (define (begin? exp) (tagged-list? exp 'begin))

          (define (begin-actions exp) (cdr exp))

          (define (last-exp? seq) (null? (cdr seq)))

          (define (first-exp seq) (car seq))

          (define (rest-exps seq) (cdr seq))

;;   We also include a constructor `sequence->exp' (for use by
;;   `cond->if') that transforms a sequence into a single expression,
;;   using `begin' if necessary:

          (define (sequence->exp seq)
            (cond ((null? seq) seq)
                  ((last-exp? seq) (first-exp seq))
                  (else (make-begin seq))))

          (define (make-begin seq) (cons 'begin seq))

;; * A procedure application is any compound expression that is not one
;;   of the above expression types.  The `car' of the expression is the
;;   operator, and the `cdr' is the list of operands:

          (define (application? exp) (pair? exp))

          (define (operator exp) (car exp))

          (define (operands exp) (cdr exp))

          (define (no-operands? ops) (null? ops))

          (define (first-operand ops) (car ops))

          (define (rest-operands ops) (cdr ops))

;;  Implementing the evaluation of `cond' in this way simplifies the
;;  evaluator because it reduces the number of special forms for which the
;;  evaluation process must be explicitly specified.

;;  We include syntax procedures that extract the parts of a `cond'
;;  expression, and a procedure `cond->if' that transforms `cond'
;;  expressions into `if' expressions.  A case analysis begins with `cond'
;;  and has a list of predicate-action clauses.  A clause is an `else'
;;  clause if its predicate is the symbol `else'.(4)

     (define (cond? exp) (tagged-list? exp 'cond))

     (define (cond-clauses exp) (cdr exp))

     (define (cond-else-clause? clause)
       (eq? (cond-predicate clause) 'else))

     (define (cond-predicate clause) (car clause))

;;     (define (cond-actions clause) (cdr clause))

;; *Exercise 4.5:*

(define (cond-actions clause)
  (if (eq? (cadr clause) '=>)
      (list (caddr clause) (cond-predicate clause))
    (cdr clause)))

     (define (cond->if exp)
       (expand-clauses (cond-clauses exp)))

;;;;;;;;;;;;;;;;;;

     (define (expand-clauses clauses)
       (if (null? clauses)
           'false                          ; no `else' clause
           (let ((first (car clauses))
                 (rest (cdr clauses)))
             (if (cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp (cond-actions first))
                     (error "ELSE clause isn't last -- COND->IF"
                            clauses))
                 (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest))))))

;;==========================

;;   *Exercise 4.4:* 

(define (and? exp) (tagged-list? exp 'and)) 
(define (and-args exp) (cdr exp)) 
(define (eval-and exp env)
  (eval (and->if exp) env)) 
(define (and->if exp)
  (expand-and (and-args exp)))
(define (expand-and args)
  (if (null? args)
      'true
    (let ((first (car args))
          (rest (cdr args)))
      (make-if (list 'not first) 'false (expand-and rest))))) 

(expand-and '((= 1 2) #t (not #f)))

(define (or? exp) (tagged-list? exp 'or)) 
(define (or-args exp) (cdr exp)) 
(define (eval-or exp env)
  (eval (or->if exp) env)) 
(define (or->if exp)
  (expand-or (or-args exp))) 
(define (expand-or args)
  (if (null? args)
      'false
    (let ((first (car args))
          (rest (cdr args)))
      (make-if first 'true (expand-or rest))))) 
