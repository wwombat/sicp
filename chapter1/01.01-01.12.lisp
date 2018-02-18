;;   *Exercise 1.1:* Below is a sequence of expressions.  What is the
;;   result printed by the interpreter in response to each expression?
;;   Assume that the sequence is to be evaluated in the order in which
;;   it is presented.

        10
;; 10

        (+ 5 3 4) 
;; 12

        (- 9 1) 
;; 8

        (/ 6 2) 
;; 3

        (+ (* 2 4) (- 4 6)) 
;; 6

        (define a 3) 
;; undefined (a, in the implementation I am using)

        (define b (+ a 1)) 
;; undefined (b)

        (+ a b (* a b)) 
;; 19 

        (= a b) 
;; #f

        (if (and (> b a) (< b (* a b)))
            b
            a) 
;; 4 (aka b)

        (cond ((= a 4) 6)
              ((= b 4) (+ 6 7 a))
              (else 25))

;; 16 (branch 2)

        (+ 2 (if (> b a) b a))
;; 6 (sub-expression resolves to b)

        (* (cond ((> a b) a)
                 ((< a b) b) ;; this one
                 (else -1))
           (+ a 1))
;; 16

;;   *Exercise 1.2:* Translate the following expression into prefix
;;   form.

;;        5 + 4 + (2 - (3 - (6 + 4/5)))
;;        -----------------------------
;;               3(6 - 2)(2 - 7)

;; Ok! First, let's calculate the expected result!
;; mmmm, let's see: -0.24666666...

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5.0)))))
   (* 3
      (- 6 2)
      (- 2 7))) 

;; Now let's check...
;; Correct!

;;   *Exercise 1.3:* Define a procedure that takes three numbers as
;;   arguments and returns the sum of the squares of the two larger
;;   numbers.

(define (>plus a b)
  (if (null? b) #t
    (> a b))) 

(define (get-two-biggest a b c)
  (define (get-biggest-2-iter biggest 2nd-biggest rest)
    (cond ((null? rest) (list biggest 2nd-biggest))
          ((>plus (car rest) biggest) (get-biggest-2-iter (car rest) biggest (cdr rest)))
          ((>plus (car rest) 2nd-biggest) (get-biggest-2-iter biggest (car rest) (cdr rest)))))
  (get-biggest-2-iter '() '() (list a b c))) 

(define (sum-of-squares-of-two-larger a b c)
  ((lambda (bigger-pair)
     (+ (square (car bigger-pair))
        (square (cadr bigger-pair))))
   (get-two-biggest a b c)))

(sum-of-squares-of-two-larger 1 2 3) ;; 13
(sum-of-squares-of-two-larger 5 2 3) ;; 34
(sum-of-squares-of-two-larger -14 2 -2) ;; 8

;;   *Exercise 1.4:* Observe that our model of evaluation allows for
;;   combinations whose operators are compound expressions.  Use this
;;   observation to describe the behavior of the following procedure:

;;        (define (a-plus-abs-b a b)
;;          ((if (> b 0) + -) a b))

;; In this procedure, the operator is chosen based on the sign of b:
;; + is b is positive, and - if b is negative.
;; This is equivalentto the definition of absolute value, if we note
;; that the - operator can be considered syntactic sugar for adding
;; that value * -1:

;; (define (- a b)
;;    (+ a (* -1 b)))

;;   *Exercise 1.5:* Ben Bitdiddle has invented a test to determine
;;   whether the interpreter he is faced with is using
;;   applicative-order evaluation or normal-order evaluation.  He
;;   defines the following two procedures:

;;      (define (p) (p))

;;      (define (test x y)
;;        (if (= x 0)
;;            0
;;            y))

;;   Then he evaluates the expression

;;      (test 0 (p))

;;   What behavior will Ben observe with an interpreter that uses
;;   applicative-order evaluation?  What behavior will he observe with
;;   an interpreter that uses normal-order evaluation?  Explain your
;;   answer.  (Assume that the evaluation rule for the special form
;;   `if' is the same whether the interpreter is using normal or
;;   applicative order: The predicate expression is evaluated first,
;;   and the result determines whether to evaluate the consequent or
;;   the alternative expression.)

;; In applicative order, the program will infinite loop,
;; since (test 0 (p)) will immediately try to evaluate the expression
;; (p).

;; However, in normal-order evaluation, also known as
;; 'fully expand and then reduce', the expression ~will~ terminate,
;; thanks to the special 'short circuiting' behavior of if!

;; In normal order, the following expansions and evaluations take place:
;; (test 0 (p))
;; (if (= 0 0) 0 (p))
;; (if #t 0 (p))
;; 0

;;   *Exercise 1.6:* Alyssa P. Hacker doesn't see why `if' needs to be
;;   provided as a special form.  "Why can't I just define it as an
;;   ordinary procedure in terms of `cond'?" she asks.  Alyssa's friend
;;   Eva Lu Ator claims this can indeed be done, and she defines a new
;;   version of `if':

;;        (define (new-if predicate then-clause else-clause)
;;          (cond (predicate then-clause)
;;                (else else-clause)))

;;   Eva demonstrates the program for Alyssa:

;;        (new-if (= 2 3) 0 5)
;;        5

;;        (new-if (= 1 1) 0 5)
;;        0

;;   Delighted, Alyssa uses `new-if' to rewrite the square-root program:

;;        (define (sqrt-iter guess x)
;;          (new-if (good-enough? guess x)
;;                  guess
;;                  (sqrt-iter (improve guess x)
;;                             x)))

;;   What happens when Alyssa attempts to use this to compute square
;;   roots?  Explain.

;; Her procedure goes into an infinite loop!
;; Since, in applicative order, all arguments to a procedure are evaluated
;; before being passed in, the next recursive call to the function is
;; ALWAYS evaluated, regardless of the result of conditional used to
;; check for termination. Functions of this form rely on the ability of
;; `if' to choose which of its arguments to evaluate based on the
;; result of a conditional expression.

;;   *Exercise 1.7:* The `good-enough?' test used in computing square
;;   roots will not be very effective for finding the square roots of
;;   very small numbers.  Also, in real computers, arithmetic operations
;;   are almost always performed with limited precision.  This makes
;;   our test inadequate for very large numbers.  Explain these
;;   statements, with examples showing how the test fails for small and
;;   large numbers.  An alternative strategy for implementing
;;   `good-enough?' is to watch how `guess' changes from one iteration
;;   to the next and to stop when the change is a very small fraction
;;   of the guess.  Design a square-root procedure that uses this kind
;;   of end test.  Does this work better for small and large numbers?

;; Explanation:

;; When the numbers are very small, the tolerance of 0.001 is very large
;; in comparison to the answer!
;; When the number get very large, in contrast, limited precision means
;; that this approach might enter an infinite loop, as we might not be able
;; to express a number that satisfies the 0.001 tolerance once our values
;; get sufficiently large.

;; ORIGINAL

     (define (sqrt-iter guess x)
       (if (good-enough? guess x)
           guess
           (sqrt-iter (improve guess x)
                      x)))

     (define (improve guess x)
       (average guess (/ x guess)))

     (define (average x y)
       (/ (+ x y) 2))

     (define (good-enough? guess x)
       (< (abs (- (square guess) x)) 0.001))

     (define (sqrt x)
       (sqrt-iter 1.0 x))

;; new test

(define (good-enough?-2 guess prev-guess)
  (display guess)
  (newline)
  (display prev-guess)
  (newline)
  (if (< (abs (- guess prev-guess)) (/ guess 1000)) #t
    #f))

(define (sqrt-iter2 guess prev-guess x)
  (if (good-enough?-2 guess prev-guess)
      guess
    (sqrt-iter2 (improve guess x) guess x))) 

(define (sqrt2 x) (sqrt-iter2 1.0 0 x)) 

(sqrt  0.00000025)
;; .031 (pretty bad!)
(sqrt2 0.00000025)
;; .00050000007665 (nice!)

(sqrt 250000000000) 
;; 50000.0
(sqrt2 250000000000) 
;; 50000.052

;;   *Exercise 1.8:* Newton's method for cube roots is based on the
;;   fact that if y is an approximation to the cube root of x, then a
;;   better approximation is given by the value

;;        x/y^2 + 2y
;;        ----------
;;            3

;;   Use this formula to implement a cube-root procedure analogous to
;;   the square-root procedure.  (In section *Note 1-3-4:: we will see
;;   how to implement Newton's method in general as an abstraction of
;;   these square-root and cube-root procedures.)

(define (newton-iter guess prev-guess x count)
  (if (> count 100) -1
    (if (good-enough?-2 guess prev-guess)
        guess
      (newton-iter (newton-improve guess x)
                   guess
                   x
                   (+ 1 count)))))
;; hmmm... very familiar!

(define (newton-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (newton-cube-root x)
  (newton-iter 1.0 0 x 0))

(newton-cube-root 27000000)

;;   *Exercise 1.9:* Each of the following two procedures defines a
;;   method for adding two positive integers in terms of the procedures
;;   `inc', which increments its argument by 1, and `dec', which
;;   decrements its argument by 1.

;;        (define (+ a b)
;;          (if (= a 0)
;;              b
;;              (inc (+ (dec a) b))))

;;        (define (+ a b)
;;          (if (= a 0)
;;              b
;;              (+ (dec a) (inc b))))

;;   Using the substitution model, illustrate the process generated by
;;   each procedure in evaluating `(+ 4 5)'.  Are these processes
;;   iterative or recursive?

;; first function!

;; (+ 4 5)
;; (if (= 4 0) b (inc (+ (dec 4) 5)))
;; (if #f b (inc (+ (dec 4) 5)))
;; (inc (+ (dec 4) 5))
;; (inc (+ 3 5))
;; (inc (if (= 3 0) 3 (inc (+ (dec 3) 5))))
;; (inc (if #f 3 (inc (+ (dec 3) 5))))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; ...
;; (inc (inc (inc (+ 1 5))))
;; ...
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc (if (= 0 0) 5 (inc (+ (dec 0) 5)))))))
;; (inc (inc (inc (inc (if #t 5 (inc (+ (dec 0) 5)))))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; Recursive! The stack 'remembers' and 'waits' for what is coming.

;; The second

;; (+ 4 5)
;; (if (= 4 0) 5 (+ (dec 4) (inc 5)))
;; (if #f 5 (+ (dec 4) (inc 5)))
;; (+ (dec 4) (inc 5))
;; (+ 3 6)
;; (if (= 3 0) 6 (+ (dec 3) (inc 6)))
;; (if #f 6 (+ (dec 3) (inc 6)))
;; (+ (dec 3) (inc 6))
;; (+ 2 7)
;; ...
;; (+ 1 8)
;; ...
;; (+ 0 9)
;; (if (= 0 0) 9 (+ (dec 0) (inc 9)))
;; (if #t 9 (+ (dec 0) (inc 9)))
;; 9

;; Iterative! The whole expression gets 'replaced' by another
;; call at each step- the stack is memoriless: the arguments 'remember'.

;;   *Exercise 1.10:* The following procedure computes a mathematical
;;   function called Ackermann's function.

        (define (A x y)
          (cond ((= y 0) 0)
                ((= x 0) (* 2 y))
                ((= y 1) 2)
                (else (A (- x 1)
                         (A x (- y 1))))))

;;   What are the values of the following expressions?

;;=====================
;;        (A 1 10) 

;; (A 1 10) 

;; (cond ((= 10 0) 0)
;;       ((= 1 0) (* 2 10)
;;       ((= 10 1) 2)
;;       (else (A (- 1 1)
;;                (A 1 (- 10 1))))))

;; (A (- 1 1)
;;    (A 1 (- 10 1)))

;; (A 0 (A 1 9))
;; ...
;; (A 0 (A 0 (A 1 8))) 
;; ...
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))) 

;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
;;    (cond ((= 1 1) (* 2 1)) ...)
;; ))))))))))

;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))) 
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4)))))))) 
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8))))))) 
;; ...
;; (A 0 512) 
;; 1024

;; (A 1 N) -> 2 ^ (N + 1)

;;=====================
;;        (A 2 4) 

        (define (A x y)
          (cond ((= y 0) 0)
                ((= x 0) (* 2 y))
                ((= y 1) 2)
                (else (A (- x 1)
                         (A x (- y 1))))))

;; (A 2 4) 

;; (cond ((= 4 0) 0)
;;       ((= 2 0) (* 2 4)
;;       ((= 4 1) 2)
;;       (else (A (- 2 1)
;;                (A 2 (- 4 1))))))

;; (A 1 (A 2 3))
;; ...
;; (A 1 (A 1 (A 2 2))
;; ...
;; (A 1 (A 1 (A 2 2))
;; ...
;; (A 1 (A 1 (A 1 (A 2 1)))) 
;;
;; (A 1 (A 1 (A 1 2))) 
;; 65536

;; From this pattern, and given what we know (A 1 N) to be,
;; we can note the following:

;; (A 1 N) -> 2 ^ (N + 1) 

;; (A 2 N) -> f(n) = 2 ^ (f(n - 1)), f(1) = 2
;; aka,
;; (A 2 N) -> (N + 1) tetration-base-2

;;=====================
;;        (A 3 3)  
;; 65536

;;   Consider the following procedures, where `A' is the procedure
;;   defined above:

;;        (define (f n) (A 0 n))

;; 2N

;;        (define (g n) (A 1 n))

;; 2^N

;;        (define (h n) (A 2 n))

;; N tetration 2
;; i.e., (2^(2^2^...n times...2))

;;        (define (k n) (* 5 n n))

;;   Give concise mathematical definitions for the functions computed
;;   by the procedures `f', `g', and `h' for positive integer values of
;;   n.  For example, `(k n)' computes 5n^2.

;;   *Exercise 1.11:* A function f is defined by the rule that f(n) = n
;;   if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3.
;;   Write a procedure that computes f by means of a recursive process.
;;   Write a procedure that computes f by means of an iterative
;;   process.

;; f(n) == n 

;; recursive
(define (1.11fun n)
  (cond ((< n 3) n)
        (else (+ (1.11fun (- n 1))
                 (* 2 (1.11fun (- n 2)))
                 (* 3 (1.11fun (- n 3)))))))

(1.11fun 6)

;; As often is the case, the iterative solution starts from
;; 'the opposite end' as the recursive one!

(define (1.11fun-iter n)
  (define (helper n n+1 n+2 count)
    (if (= 0 count) n
      (helper n+1 n+2 (+ n+2 (* 2 n+1) (* 3 n)) (- count 1))))
  (helper 0 1 2 n))

(1.11fun-iter 6) 

;;   *Exercise 1.12:* The following pattern of numbers is called "Pascal's
;;   triangle".

;;                1
;;              1   1
;;            1   2   1
;;          1   3   3   1
;;        1   4   6   4   1

;;   The numbers at the edge of the triangle are all 1, and each number
;;   inside the triangle is the sum of the two numbers above it.(4)
;;   Write a procedure that computes elements of Pascal's triangle by
;;   means of a recursive process.

(define (pascal row index)
  (cond ((= index 0) 1)
        ((= row index) 1)
        (else (+ (pascal (- row 1) index)
                 (pascal (- row 1) (- index 1))))))

(pascal 4 1) 
