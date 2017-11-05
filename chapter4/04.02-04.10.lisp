

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

;; While what they are looking for is something along the lines of
;; the following: 

    (put ('lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                    (lambda-body exp)
                                                    env))))

    ;; etc...

    (define (dd-eval exp env)
    (let ((action (get (car exp))))
        (cond ((not (null? action)) (action exp env))
            (else (error "Unknown expression type -- EVAL" exp)))))

;; It is worth nothing that this implementation requires, as the authors
;; point out above, a grammar where every different kind of expression is
;; preceeded by a lable indicating which type of expression it is!
;; While this makes our dispatch quick - an amortized constant time, in-
;; dependant of the number of expression types, if we're using a hash
;; table!! - it is hardly the most ergonomic choice!
;;
;; We could compensate for this by splitting up the expression type
;; identification step and the evaluation step, transforming our initial
;; expressions into a tagged form as a first step of interpretation-
;; but then we are encountered with the same problem again, of how to
;; dispatch on expression type in an efficient way!

;; The book's implementation (a long cond clause) has a number of checks
;; proportional to the number of expression types- and it is easy to
;; imagine arranging those checks in such a way that additional expres-
;; sion types could be added without editing the evaluate method!

;; Instead, we could create a list of operations and iterate through them!
;; The definition of eval, along with the addition of expression types,
;; would then look something like this:

;; Define a pair object that contains a 

(define (make-expr-type predicate evaluator)
  (cons predicate evaluator))

(define (expr-pred exp-type)
  (car exp-type))

(define (expr-eval exp-type)
  (cdr exp-type))

(define expressions '())

(define (add-expression expression)
  (set! expressions (append expressions (list expression))))

(define (list-eval exp env)
  (define (list-eval-iter exp env expressions)
    (cond ((null? expressions) (error "Unknown expression type -- EVAL" exp))
          (((expr-pred (car expressions)) exp) ((expr-eval (car expressions)) exp env))
          (else (list-eval-iter exp env (cdr expressions)))))
  (list-eval-iter exp env expressions)) 

;; Example

;; recalling tagged-list? ...

      (define (tagged-list? exp tag)
        (if (pair? exp)
            (eq? (car exp) tag)
            false))

(define (is-potato? expr) (tagged-list? expr 'potato)) 
(define (eval-potato expr env) 42)

(define (is-parsnip? expr) (tagged-list? expr 'parsnip)) 
(define (eval-parsnip expr env) 1)

(add-expression (make-expr-type is-potato? eval-potato)) 
(add-expression (make-expr-type is-parsnip? eval-parsnip)) 

(list-eval '(potato + 5) '()) 
(list-eval '(parsnip + 5) '()) 
(list-eval '(3 + 5) '()) 

;; A more sophisticated strategy, perhaps, could search this list of
;; predicate in a more efficient way- through a tree structure, perhaps...
;; But figuring out how to generically construct a thing is surely beyond
;; the scope of this assignment!


;;   *Exercise 4.4:* Recall the definitions of the special forms `and'
;;   and `or' from *Note Chapter 1:::
;;
;;      * `and': The expressions are evaluated from left to right.  If
;;        any expression evaluates to false, false is returned; any
;;        remaining expressions are not evaluated.  If all the
;;        expressions evaluate to true values, the value of the last
;;        expression is returned.  If there are no expressions then
;;        true is returned.
;;
;;      * `or': The expressions are evaluated from left to right.  If
;;        any expression evaluates to a true value, that value is
;;        returned; any remaining expressions are not evaluated.  If
;;        all expressions evaluate to false, or if there are no
;;        expressions, then false is returned.
;;
;;   Install `and' and `or' as new special forms for the evaluator by
;;   defining appropriate syntax procedures and evaluation procedures
;;   `eval-and' and `eval-or'.  Alternatively, show how to implement
;;   `and' and `or' as derived expressions.

;; Approach 1: special procedures
(define (default? exp) #t)
(define (eval-default exp env) exp)

(define (is-and? exp) (tagged-list? exp 'and)) 
(define (and-args exp) (cdr exp)) 
(define (eval-and exp env)
  (define (eval-and-rec exps env)
    (if (null? exps) #t
      (and (list-eval (car exps) env) (eval-and-rec (cdr exps) env))))
  (eval-and-rec (and-args exp) env)) 

(define (is-or? exp) (tagged-list? exp 'or)) 
(define (or-args exp) (cdr exp)) 
(define (eval-or exp env)
  (define (eval-or-rec exps env)
    (if (null? exps) #f
      (or (list-eval (car exps) env) (eval-or-rec (cdr exps) env))))
  (eval-or-rec (and-args exp) env)) 

(define expressions '())

(add-expression (make-expr-type is-and? eval-and)) 
(add-expression (make-expr-type is-or? eval-or)) 
(add-expression (make-expr-type default? eval-default)) 

(list-eval '(and #t #t #f) '()) 
(list-eval '(and #t #t #t) '()) 
(list-eval '(or #f #t #f) '()) 
(list-eval '(or (and #f #f) (and #t #f)) '()) 

;;;;; not in a dumb way

;; meh! the dumb way is fine 'w'

;;;;; as derived expressions

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (is-and? exp) (tagged-list? exp 'and)) 
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

(define (is-or? exp) (tagged-list? exp 'or)) 
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
