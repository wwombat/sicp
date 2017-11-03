;;   *Exercise 2.53:* What would the interpreter print in response to
;;   evaluating each of the following expressions?

          (list 'a 'b 'c) 

;; (a b c)

          (list (list 'george))

;; ((george))

          (cdr '((x1 x2) (y1 y2)))

;; ((y1 y2))

          (cadr '((x1 x2) (y1 y2)))

;; (y1 y2)

          (pair? (car '(a short list)))

;; #f

          (memq 'red '((red shoes) (blue socks)))

;; #f

          (memq 'red '(red shoes blue socks))

;; (red shoes blue socks)

;;   *Exercise 2.54:* Two lists are said to be `equal?' if they contain
;;   equal elements arranged in the same order.  For example,

        (equal? '(this is a list) '(this is a list))

;;   is true, but

        (equal? '(this is a list) '(this (is a) list))

;;   is false.  To be more precise, we can define `equal?'  recursively
;;   in terms of the basic `eq?' equality of symbols by saying that `a'
;;   and `b' are `equal?' if they are both symbols and the symbols are
;;   `eq?', or if they are both lists such that `(car a)' is `equal?'
;;   to `(car b)' and `(cdr a)' is `equal?' to `(cdr b)'.  Using this
;;   idea, implement `equal?' as a procedure.(5)

(define (equal? a b)
  (cond ((or (not (pair? a)) (not (pair? b))) (eq? a b))
        (else (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))))

;;   *Exercise 2.55:* Eva Lu Ator types to the interpreter the
;;   expression

;;        (car ''abracadabra) 

;;   To her surprise, the interpreter prints back `quote'.  Explain.

;;  As noted in foot note (3), 

;;      The quotation mark is just a
;;      single-character abbreviation for wrapping the next complete expression
;;      with `quote' to form `(quote <EXPRESSION>)'. This is important because
;;      it maintains the principle that any expression seen by the interpreter
;;      can be manipulated as a data object. 

;; ''abracadabra can be rewritten, equivalently, as (quote (quote abracadbra)) 
;; Written in this qay, it becomes apparent that the car of this expression is
;; 'quote'!

