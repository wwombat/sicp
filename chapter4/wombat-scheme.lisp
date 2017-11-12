;; Let's save the original wombat-apply, real quick!!!
(define wombat-apply-in-underlying-scheme apply)

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
        ((switch? exp) (eval (switch->cond exp) env)) 
        ((let? exp) (eval (let->lambda exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((application? exp)
         (wombat-apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (wombat-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (wombat-apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- WOMBAT-APPLY" procedure))))

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

;;   `Quoted?' is defined in terms of the procedure `tagged-list?',
;;   which identifies lists beginning with a designated symbol:

          (define (tagged-list? exp tag)
            (if (pair? exp)
                (eq? (car exp) tag)
                false))

;; * Quotations have the form `(quote <TEXT-OF-QUOTATION>)':(1)

          (define (quoted? exp)
            (tagged-list? exp 'quote))

          (define (text-of-quotation exp) (cadr exp))

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

;;   *Exercise 4.6:* `Let' expressions are derived expressions, because

;;        (let ((<VAR_1> <EXP_1>) ... (<VAR_N> <EXP_N>))
;;          <BODY>)

;;   is equivalent to

;;        ((lambda (<VAR_1> ... <VAR_N>)
;;           <BODY>)
;;         <EXP_1>
;;         ...
;;         <EXP_N>)

(define (let? p) (tagged-list? p 'let))

(define (let-clauses expr) (cadr expr))
(define (let-body expr) (cddr expr))

(define (let-vars clauses)
  (fold-right cons '() (map let-clause-var clauses))) 

(define (let-exprs clauses)
  (fold-right cons '() (map let-clause-exp clauses))) 

(define (let-clause-var clause) (car clause))
(define (let-clause-exp clause) (cadr clause))

(define (let->lambda expr)
  (let ((clauses (let-clauses expr)))
    (cons (make-lambda (let-vars clauses) (let-body expr))
          (let-exprs clauses)))) 


;; custom! (exercise 4.10?)
;; switch statement has the following behavior:
;;
;; (switch <tag-expr>
;;    ((<name1> <value1>)
;;     (<name2> <value2>)
;;     (else <value3>)))
;;
;; If the result of evaluating tag-expr matches one
;; of the listed <name> values, return the specified
;; value, evaluated. Otherwise, if an else statement
;; is specified, return the value associated with the
;; else- if not, return '().

;; (define (make-animal species name) (cons species name))
;; (define dog (make-animal 'dog 'steve))
;; (define cat (make-animal 'cat 'sally))
;; (define fish (make-animal 'fish 'mortimer))
;; (switch 'dog (('dog 1) ('cat 2) (else 3)))
;;    

(define (switch? p) (tagged-list? p 'switch))
(define (switch-tag-name exp) (cadr exp)) 
(define (switch-clauses exp) (caddr exp)) 
(define (switch-predicate clause) (car clause))
(define (switch-value clause) (cdr clause))
(define (switch-else-clause? clause)
  (eq? (switch-predicate clause) 'else))

;; I implement it as a derived expression:
;; a switch statement can be transformed into a
;; cond statement of the following form, assuming
;; the presence of a built-in eq? operator:
;;

(define (make-cond clauses)
  (cons 'cond clauses))
(define (make-cond-clause pred val)
  (cons pred val))

(define (switch->cond expr)
  (define (switch-clauses->cond-clauses tag clauses)
    (define (switch-clause->cond-clause tag clause)
      (if (switch-else-clause? clause)
          (make-cond-clause 'else (switch-value clause))
        (make-cond-clause (list 'eq? tag (switch-predicate clause))
                          (switch-value clause))))
    (if (null? clauses) '()
      (cons (switch-clause->cond-clause tag (car clauses))
            (switch-clauses->cond-clauses tag (cdr clauses))))) 
  (let ((tag-name (switch-tag-name expr))
        (clauses (switch-clauses expr)))
    (make-cond (switch-clauses->cond-clauses tag-name clauses))))

;; TO DO:

;;   *Exercise 4.8:* `Let*'
;;   *Exercise 4.8:* "Named `let'"
;;   *Exercise 4.9:* Many languages support a variety of iteration
;;   constructs, such as `do', `for', `while', and `until'. 
;;   *Exercise 4.10:* By using data abstraction, we were able to write
;;   an `eval' procedure that is independent of the particular syntax
;;   of the language to be evaluated.  To illustrate this, design and
;;   implement a new syntax for Scheme by modifying the procedures in
;;   this section, without changing `eval' or `wombat-apply'.

;;===============================================
;; EVALUATOR DATA STRUCTURES
;;===============================================

;;  For conditionals, we accept anything to be true that is not the explicit
;;  `false' object.

     (define (true? x)
       (not (eq? x false)))

     (define (false? x)
       (eq? x false))

(define (wombat-not x)
  (if (true? x) false true))

;;  Compound procedures are constructed from parameters, procedure
;;  bodies, and environments using the constructor `make-procedure':

     (define (make-procedure parameters body env)
       (list 'procedure parameters body env))

     (define (compound-procedure? p)
       (tagged-list? p 'procedure))

     (define (procedure-parameters p) (cadr p))

     (define (procedure-body p) (caddr p))

     (define (procedure-environment p) (cadddr p))


;;  To implement these operations we represent an environment as a list
;;  of frames.  The enclosing environment of an environment is the `cdr' of
;;  the list.  The empty environment is simply the empty list.

     (define (enclosing-environment env) (cdr env))

     (define (first-frame env) (car env))

     (define the-empty-environment '())

;;  Each frame of an environment is represented as a pair of lists: a
;;  list of the variables bound in that frame and a list of the associated
;;  values.(1)

     (define (make-frame variables values)
       (cons variables values))

     (define (frame-variables frame) (car frame))

     (define (frame-values frame) (cdr frame))

     (define (add-binding-to-frame! var val frame)
       (set-car! frame (cons var (car frame)))
       (set-cdr! frame (cons val (cdr frame))))

;;  To extend an environment by a new frame that associates variables
;;  with values, we make a frame consisting of the list of variables and
;;  the list of values, and we adjoin this to the environment.  We signal
;;  an error if the number of variables does not match the number of values.

     (define (extend-environment vars vals base-env)
       (if (= (length vars) (length vals))
           (cons (make-frame vars vals) base-env)
           (if (< (length vars) (length vals))
               (error "Too many arguments supplied" vars vals)
               (error "Too few arguments supplied" vars vals))))

;;  To look up a variable in an environment, we scan the list of
;;  variables in the first frame.  If we find the desired variable, we
;;  return the corresponding element in the list of values.  If we do not
;;  find the variable in the current frame, we search the enclosing
;;  environment, and so on.  If we reach the empty environment, we signal
;;  an "unbound variable" error.

     (define (lookup-variable-value var env)
       (define (env-loop env)
         (define (scan vars vals)
           (cond ((null? vars)
                  (env-loop (enclosing-environment env)))
                 ((eq? var (car vars))
                  (car vals))
                 (else (scan (cdr vars) (cdr vals)))))
         (if (eq? env the-empty-environment)
             (error "Unbound variable" var)
             (let ((frame (first-frame env)))
               (scan (frame-variables frame)
                     (frame-values frame)))))
       (env-loop env))

;;  To set a variable to a new value in a specified environment, we scan
;;  for the variable, just as in `lookup-variable-value', and change the
;;  corresponding value when we find it.

     (define (set-variable-value! var val env)
       (define (env-loop env)
         (define (scan vars vals)
           (cond ((null? vars)
                  (env-loop (enclosing-environment env)))
                 ((eq? var (car vars))
                  (set-car! vals val))
                 (else (scan (cdr vars) (cdr vals)))))
         (if (eq? env the-empty-environment)
             (error "Unbound variable -- SET!" var)
             (let ((frame (first-frame env)))
               (scan (frame-variables frame)
                     (frame-values frame)))))
       (env-loop env))

;;  To define a variable, we search the first frame for a binding for
;;  the variable, and change the binding if it exists (just as in
;;  `set-variable-value!').  If no such binding exists, we adjoin one to
;;  the first frame.

     (define (define-variable! var val env)
       (let ((frame (first-frame env)))
         (define (scan vars vals)
           (cond ((null? vars)
                  (add-binding-to-frame! var val frame))
                 ((eq? var (car vars))
                  (set-car! vals val))
                 (else (scan (cdr vars) (cdr vals)))))
         (scan (frame-variables frame)
               (frame-values frame))))

;;===============================================
;; RUNNING THE EVALUATOR
;;===============================================

;;  It does not matter how we represent the primitive procedure objects,
;;  so long as `wombat-apply' can identify and wombat-apply them by using the procedures
;;  `primitive-procedure?' and `wombat-apply-primitive-procedure'.  We have chosen
;;  to represent a primitive procedure as a list beginning with the symbol
;;  `primitive' and containing a procedure in the underlying Lisp that
;;  implements that primitive.

     (define (primitive-procedure? proc)
       (tagged-list? proc 'primitive))

     (define (primitive-implementation proc) (cadr proc))

;;  `Setup-environment' will get the primitive names and implementation
;;  procedures from a list:(1)

     (define primitive-procedures
       (list (list 'car car)
             (list 'cdr cdr)
             (list 'cons cons)
             (list 'null? null?)
             (list '+ +)
             (list '- -)
             (list '* *)
             (list '/ /)
             (list 'modulo modulo)
             (list 'exp exp)
             (list 'not wombat-not)
             (list 'eq? eq?)
             (list '= =)
             ))

     (define (primitive-procedure-names)
       (map car
            primitive-procedures))

     (define (primitive-procedure-objects)
       (map (lambda (proc) (list 'primitive (cadr proc)))
            primitive-procedures))

;;  To wombat-apply a primitive procedure, we simply wombat-apply the implementation
;;  procedure to the arguments, using the underlying Lisp system:(2)

     (define (wombat-apply-primitive-procedure proc args)
       (wombat-apply-in-underlying-scheme
        (primitive-implementation proc) args))

;;  There must be a binding for each primitive procedure name, so that
;;  when `eval' evaluates the operator of an application of a primitive, it
;;  will find an object to pass to `wombat-apply'.  We thus set up a global
;;  environment that associates unique objects with the names of the
;;  primitive procedures that can appear in the expressions we will be
;;  evaluating.  The global environment also includes bindings for the
;;  symbols `true' and `false', so that they can be used as variables in
;;  expressions to be evaluated.

     (define (setup-environment)
       (let ((initial-env
              (extend-environment (primitive-procedure-names)
                                  (primitive-procedure-objects)
                                  the-empty-environment)))
         (define-variable! 'true true initial-env)
         (define-variable! 'false false initial-env)
         initial-env))

     (define the-global-environment (setup-environment)) 

;;  For convenience in running the metacircular evaluator, we provide a "driver
;;  loop" that models the read-eval-print loop of the underlying Lisp
;;  system.  It prints a "prompt", reads an input expression, evaluates
;;  this expression in the global environment, and prints the result.  We
;;  precede each printed result by an "output prompt" so as to distinguish
;;  the value of the expression from other output that may be printed.(3)

     (define input-prompt ";;; M-Eval input :>") 
     (define output-prompt ";;; M-Eval value :3")

     (define (driver-loop)
       (prompt-for-input input-prompt)
       (let ((input (read)))
         (let ((output (eval input the-global-environment)))
           (announce-output output-prompt)
           (user-print output)))
       (driver-loop))

     (define (prompt-for-input string)
       (newline) (newline) (display string) (newline))

     (define (announce-output string)
       (newline) (display string) (newline))

;;  We use a special printing procedure, `user-print', to avoid printing
;;  the environment part of a compound procedure, which may be a very long
;;  list (or may even contain cycles).

     (define (user-print object)
       (if (compound-procedure? object)
           (display (list 'compound-procedure
                          (procedure-parameters object)
                          (procedure-body object)
                          '<procedure-env>))
           (display object)))

