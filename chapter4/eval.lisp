;; Eval

;;  For clarity, `eval' has been implemented as a case analysis using
;;  `cond'.  The disadvantage of this is that our procedure handles only a
;;  few distinguishable types of expressions, and no new ones can be
;;  defined without editing the definition of `eval'.  In most Lisp
;;  implementations, dispatching on the type of an expression is done in a
;;  data-directed style.  This allows a user to add new types of
;;  expressions that `eval' can distinguish, without modifying the
;;  definition of `eval' itself.  (See *Note Exercise 4-3::.)

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
