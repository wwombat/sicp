
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
           (cons (eval (first-operand exps) env)
                 (list-of-values (rest-operands exps) env))))

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

;;   *Exercise 4.1:* Notice that we cannot tell whether the
;;   metacircular evaluator evaluates operands from left to right or
;;   from right to left.  Its evaluation order is inherited from the
;;   underlying Lisp: If the arguments to `cons' in `list-of-values'
;;   are evaluated from left to right, then `list-of-values' will
;;   evaluate operands from left to right; and if the arguments to
;;   `cons' are evaluated from right to left, then `list-of-values'
;;   will evaluate operands from right to left.

;;   Write a version of `list-of-values' that evaluates operands from
;;   left to right regardless of the order of evaluation in the
;;   underlying Lisp.  Also write a version of `list-of-values' that
;;   evaluates operands from right to left.

(define (left-to-right-list-of-values exps env)
  (if (no-operands? exps)
      '()
    (let ((left (eval (first-operand exps) env)))
      (let ((right (list-of-values (rest-operands exps) env)))
        (define left (eval (first-operand exps) env))
        (cons left right)))))

(define (left-to-right-list-of-values exps env)
  (if (no-operands? exps)
      '()
    (let ((right (list-of-values (rest-operands exps) env)))
      (let ((left (eval (first-operand exps) env)))
        (define left (eval (first-operand exps) env))
        (cons left right)))))

;; and a little proof of concept to make sure this
;; approach actually works...

(define (poc1)
  (set! x 3)
  (let ((first (set! x (+ 1 x))))
    (let ((second (set! x (* 2 x))))
      x))) 

(define (poc2)
  (set! x 3)
  (let ((second (set! x (* 2 x))))
    (let ((first (set! x (+ 1 x))))
      x))) 

(poc1) 
;; 8
(poc2) 
;; 7

;; Looks good to me!

