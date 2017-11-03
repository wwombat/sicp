

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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;   *Exercise 4.2:* Louis Reasoner plans to reorder the `cond' clauses
;;   in `eval' so that the clause for procedure applications appears
;;   before the clause for assignments.  He argues that this will make
;;   the interpreter more efficient: Since programs usually contain more
;;   applications than assignments, definitions, and so on, his
;;   modified `eval' will usually check fewer clauses than the original
;;   `eval' before identifying the type of an expression.

;;     a. What is wrong with Louis's plan?  (Hint: What will Louis's
;;        evaluator do with the expression `(define x 3)'?)

;; Recall our definition and implemention of 'application'!

    ;; * A procedure application is any compound expression that is not one
    ;;   of the above expression types.  The `car' of the expression is the
    ;;   operator, and the `cdr' is the list of operands:

;; Since an application is, definitionally 'not one of the above expression
;; types', then it is basically ~required~ that we perform these checks
;; first! Using our implementations of these checks unchanged, a call to
;; '(define x 3) will check the environment for a function named 'define'-
;; which, since we haven't provided one here, is hardly what we want!
;; To make his program work, in "application?" we would either have to check
;; whether this expression being applied is a keyword - AKA, doing the very
;; checks we are attempting to avoid! -, OR, do some sort of crazy white-list
;; of every other possible term, which, since that space is practically
;; infinite, is entirely impossible.

;;     b. Louis is upset that his plan didn't work.  He is willing to
;;        go to any lengths to make his evaluator recognize procedure
;;        applications before it checks for most other kinds of
;;        expressions.  Help him by changing the syntax of the
;;        evaluated language so that procedure applications start with
;;        `call'.  For example, instead of `(factorial 3)' we will now
;;        have to write `(call factorial 3)' and instead of `(+ 1 2)'
;;        we will have to write `(call + 1 2)'.

;; To implement this, we simply have to change the application call
;; to check for our new keyword 'call', and then shift all our other
;; checks one cdr down the list.

(define (louis-application? exp) (eq? (car exp) 'call))

(define (louis-operator exp) (cadr exp))

(define (louis-operands exp) (cddr exp))

(define (louis-no-operands? ops) (null? ops))

(define (louis-first-operand ops) (car ops))

(define (louis-rest-operands ops) (cdr ops))

(define louis-test-case '(call call-home "hi mom!" lots-of-love)) 
(louis-application? louis-test-case) ;; #t
(louis-operator louis-test-case) ;; 'call-home
(louis-operands louis-test-case) ;; '("hi mom!" lots-of-love) 

;;   *Exercise 4.3:* Rewrite `eval' so that the dispatch is done in
;;   data-directed style.  Compare this with the data-directed
;;   differentiation procedure of *Note Exercise 2-73::.  (You may use
;;   the `car' of a compound expression as the type of the expression,
;;   as is appropriate for the syntax implemented in this section.)

     (define (make-table same-key?)
       (let ((local-table (list '*table*)))
         (define (assoc key records)
           (cond ((null? records) #f)
                 ((same-key? key (caar records)) (car records))
                 (else (assoc key (cdr records))))) 
         (define (lookup key-1 key-2)
           (let ((subtable (assoc key-1 (cdr local-table))))
             (if subtable
                 (let ((record (assoc key-2 (cdr subtable))))
                   (if record
                       (cdr record)
                       false))
                 false)))
         (define (insert! key-1 key-2 value)
           (let ((subtable (assoc key-1 (cdr local-table))))
             (if subtable
                 (let ((record (assoc key-2 (cdr subtable))))
                   (if record
                       (set-cdr! record value)
                       (set-cdr! subtable
                                 (cons (cons key-2 value)
                                       (cdr subtable)))))
                 (set-cdr! local-table
                           (cons (list key-1
                                       (cons key-2 value))
                                 (cdr local-table)))))
           'ok)
         (define (dispatch m)
           (cond ((eq? m 'lookup-proc) lookup)
                 ((eq? m 'insert-proc!) insert!)
                 (else (error "Unknown operation -- TABLE" m))))
         dispatch))

     (define operation-table (make-table eq?))
     (define get (operation-table 'lookup-proc))
     (define put (operation-table 'insert-proc!))

;; ================

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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))



;;   *Exercise 4.4:* Recall the definitions of the special forms `and'
;;   and `or' from *Note Chapter 1:::

;;      * `and': The expressions are evaluated from left to right.  If
;;        any expression evaluates to false, false is returned; any
;;        remaining expressions are not evaluated.  If all the
;;        expressions evaluate to true values, the value of the last
;;        expression is returned.  If there are no expressions then
;;        true is returned.

;;      * `or': The expressions are evaluated from left to right.  If
;;        any expression evaluates to a true value, that value is
;;        returned; any remaining expressions are not evaluated.  If
;;        all expressions evaluate to false, or if there are no
;;        expressions, then false is returned.


;;   Install `and' and `or' as new special forms for the evaluator by
;;   defining appropriate syntax procedures and evaluation procedures
;;   `eval-and' and `eval-or'.  Alternatively, show how to implement
;;   `and' and `or' as derived expressions.

;;   *Exercise 4.5:* Scheme allows an additional syntax for `cond'
;;   clauses, `(<TEST> => <RECIPIENT>)'.  If <TEST> evaluates to a true
;;   value, then <RECIPIENT> is evaluated.  Its value must be a
;;   procedure of one argument; this procedure is then invoked on the
;;   value of the <TEST>, and the result is returned as the value of
;;   the `cond' expression.  For example

;;        (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;              (else false))

;;   returns 2.  Modify the handling of `cond' so that it supports this
;;   extended syntax.

;;   *Exercise 4.6:* `Let' expressions are derived expressions, because

;;        (let ((<VAR_1> <EXP_1>) ... (<VAR_N> <EXP_N>))
;;          <BODY>)

;;   is equivalent to

;;        ((lambda (<VAR_1> ... <VAR_N>)
;;           <BODY>)
;;         <EXP_1>
;;         ...
;;         <EXP_N>)

;;   Implement a syntactic transformation `let->combination' that
;;   reduces evaluating `let' expressions to evaluating combinations of
;;   the type shown above, and add the appropriate clause to `eval' to
;;   handle `let' expressions.

;;   *Exercise 4.7:* `Let*' is similar to `let', except that the
;;   bindings of the `let' variables are performed sequentially from
;;   left to right, and each binding is made in an environment in which
;;   all of the preceding bindings are visible.  For example

;;        (let* ((x 3)
;;               (y (+ x 2))
;;               (z (+ x y 5)))
;;          (* x z))

;;   returns 39.  Explain how a `let*' expression can be rewritten as a
;;   set of nested `let' expressions, and write a procedure
;;   `let*->nested-lets' that performs this transformation.  If we have
;;   already implemented `let' (*Note Exercise 4-6::) and we want to
;;   extend the evaluator to handle `let*', is it sufficient to add a
;;   clause to `eval' whose action is

;;        (eval (let*->nested-lets exp) env)

;;   or must we explicitly expand `let*' in terms of non-derived
;;   expressions?

;;   *Exercise 4.8:* "Named `let'" is a variant of `let' that has the
;;   form

;;        (let <VAR> <BINDINGS> <BODY>)

;;   The <BINDINGS> and <BODY> are just as in ordinary `let', except
;;   that <VAR> is bound within <BODY> to a procedure whose body is
;;   <BODY> and whose parameters are the variables in the <BINDINGS>.
;;   Thus, one can repeatedly execute the <BODY> by invoking the
;;   procedure named <VAR>.  For example, the iterative Fibonacci
;;   procedure (section *Note 1-2-2::) can be rewritten using named
;;   `let' as follows:

;;        (define (fib n)
;;          (let fib-iter ((a 1)
;;                         (b 0)
;;                         (count n))
;;            (if (= count 0)
;;                b
;;                (fib-iter (+ a b) a (- count 1)))))

;;   Modify `let->combination' of *Note Exercise 4-6:: to also support
;;   named `let'.

;;   *Exercise 4.9:* Many languages support a variety of iteration
;;   constructs, such as `do', `for', `while', and `until'.  In Scheme,
;;   iterative processes can be expressed in terms of ordinary
;;   procedure calls, so special iteration constructs provide no
;;   essential gain in computational power.  On the other hand, such
;;   constructs are often convenient.  Design some iteration
;;   constructs, give examples of their use, and show how to implement
;;   them as derived expressions.

;;   *Exercise 4.10:* By using data abstraction, we were able to write
;;   an `eval' procedure that is independent of the particular syntax
;;   of the language to be evaluated.  To illustrate this, design and
;;   implement a new syntax for Scheme by modifying the procedures in
;;   this section, without changing `eval' or `apply'.
