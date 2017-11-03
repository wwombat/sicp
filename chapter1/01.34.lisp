;;   *Exercise 1.34:* Suppose we define the procedure

          (define (f g)
            (g 2))

;;   Then we have

;;        (f square)
;;        4

;;        (f (lambda (z) (* z (+ z 1))))
;;        6

;;   What happens if we (perversely) ask the interpreter to evaluate
;;   the combination `(f f)'?  Explain.

;;   Presemuably it will attempt to evaluate 2 and error out,
;;   have performed the following evaluations:
;;
;;   1.  (f f)
;;   2.  (f 2)
;;   3.  (2 2) ??? bad
;;
;;   Let us confirm.

(f f) 

;; Indeed, copied from my precious REPL:
;;
;; 44 error> (f f)
;; ;The object 2 is not applicable.
;;
;; Moving on...
