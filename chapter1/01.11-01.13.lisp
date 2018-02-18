;; Challenge... TODO: Write an iterative implementation!

     (define (count-change amount)
       (cc amount 5))

     (define (cc amount kinds-of-coins)
       (cond ((= amount 0) 1)
             ((or (< amount 0) (= kinds-of-coins 0)) 0)
             (else (+ (cc amount
                          (- kinds-of-coins 1))
                      (cc (- amount
                             (first-denomination kinds-of-coins))
                          kinds-of-coins)))))

     (define (first-denomination kinds-of-coins)
       (cond ((= kinds-of-coins 1) 1)
             ((= kinds-of-coins 2) 5)
             ((= kinds-of-coins 3) 10)
             ((= kinds-of-coins 4) 25)
             ((= kinds-of-coins 5) 50)))

(count-change 100)

;;   *Exercise 1.11:* A function f is defined by the rule that f(n) = n
;;   if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3.
;;   Write a procedure that computes f by means of a recursive process.
;;   Write a procedure that computes f by means of an iterative
;;   process.

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3)))))))

(define (f2 n)
  (cond ((< n 3) n)
        (else (f2-iter 4 2 1 (- n 3)))))

(define (f2-iter val prev prev-prev count)
  (if (= count 0)
      val
    (f2-iter (+ val
                 (* 2 prev)
                 (* 3 prev-prev)) 
              val
              prev
              (- count 1))))
 

(f 2) 
(f2 2) 

(f 3) 
(f2 3) 

(f 4) 
(f2 4) 

(f 5) 
(f2 5) 

(f 6) 
(f2 6) 

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

(define (abs x)
  (if (< x 0)
      (- 0 x)
    x))

(define (<= x y) (or (< x y) (= x y)))

(define (pasc L P)
  (cond
   ((< L 2) 1)
   ((= L P) 1)
   ((= 1 P) 1)
   (else (+ (pasc (- L 1) (- P 1)) (pasc (- L 1) P)))))

(pasc 1 1)

(pasc 5 2) 

;;   *Exercise 1.13:* Prove that _Fib_(n) is the closest integer to
;;   [phi]^n/[sqrt](5), where [phi] = (1 + [sqrt](5))/2.  Hint: Let
;;   [illegiblesymbol] = (1 - [sqrt](5))/2.  Use induction and the
;;   definition of the Fibonacci numbers (see section *Note 1-2-2::) to
;;   prove that _Fib_(n) = ([phi]^n - [illegiblesymbol]^n)/[sqrt](5).

Proposition: fib(n) is the closest integer to PHI^n/sqrt(5)

--- THE POOF ---

     *Exercise 1.13:* Prove that fib(n) is the closest integer to PHI^n/sqrt(5),
     where PHI = (1 + sqrt(5)) / 2.

     Hint: Let CHI = (1 - sqrt(5))/2.  Use induction and the definition of the
     Fibonacci numbers (see [below]) to prove that _Fib_(n) = (PHI^n - CHI^n) /
     sqrt(5).

Definition:
                 .
                |  0    -> 0
     fib(n)  = <   1    -> 1
                |  else -> fib(n - 1) + fib(n - 2)
                 .

Define PHI and CHI such that

    PHI = (1 + sqrt(5)) / 2
    CHI = (1 - sqrt(5)) / 2

We note an interesting property of both values:

    PHI^2 = (1 + sqrt(5)) * (1 + sqrt(5)) / 4
    PHI^2 = (1 + 2sqrt(5) + 5) / 4
    PHI^2 = (6 + 2sqrt(5)) / 4
    PHI^2 = (3 + sqrt(5)) / 2 = PHI + 1

    CHI^2 = (1 - sqrt(5)) * (1 - sqrt(5)) / 4
    CHI^2 = (1 - 2sqrt(5) + 5) / 4
    CHI^2 = (6 - 2sqrt(5)) / 4
    CHI^2 = (3 - sqrt(5)) / 2 = CHI + 1

These values are, it turns out, the two solutions to x^2 = x + 1 !

Given two constants a and b, let's consider the value:

    Val1 = (PHI * b) + a

Multiplying this by PHI, we find

    (PHI * (PHI * b)) + (PHI * a)
    (PHI^2 * b) + (PHI * a)
    (PHI + 1) * b + (PHI * a)
    (PHI * b) + b + (PHI * a)
    (PHI * (a + b)) + b

Defining c = a + b, we get

    Val2 = (PHI * c) + b = Val1 * PHI

Eerily familiar! Multiplying by PHI again and defining d as (c + b),
we get:

    Val3 = (PHI * d) + c = Val2 * PHI

And so on and so forth...

Let us, for fun's sake, expand all of these expressions in terms of
a and b!

    Val1 = (PHI * b) + a

    Val2 = (PHI * c) + b
    Val2 = (PHI * (b + a)) + b
    Val2 = (PHI * b) + (PHI * a) + b

    Val3 = (PHI * d) + c 
    Val3 = (PHI * (c + b)) + (a + b)
    Val3 = (PHI * (a + b + b)) + (a + b)
    Val3 = (PHI * (2b + a)) + a + b

Hmmmm... But what is this?

    Val3 = (PHI * 2b) + (PHI * a) + a + b
    Val3 = (PHI * b) + (PHI * b) + (PHI * a) + a + b
    Val3 = ((PHI * b) + (PHI * a) + b) + ((PHI * b) + a)
    Val3 = Val2 + Val1

    AKA

    PHI^(n + 2) = PHI^(n + 1) + PHI^(  n  )

Note that this relationship applies equally, of course, to our friend CHI,
defined above. 

Wow wow wow!!! 
One might intuit that we are, at this point, very close indeed to finding a way to
prove a firm connection between this function and the fibonacci function!     
It is hard to miss the fact that this recurrence is suspiciously similar to the
3rd case of our fib definition... One is forced to ask oneself - could it be???
Are either of these functions, in fact, the Fibonacci function in disguise???

Unfortunately, it turns out that neither is... While they both fulfill
the third part of the definition (that n > 1 -> fib(n - 1) + fib(n - 2)!), they
fail to deliver on the first two counts (that n = 0 -> 0, and n = 1 -> 1)...
To find a function that matches those, we'll have to dig a little deeper...

    (PHI^(0) - CHI^(0)) / sqrt(5)

Now, we examine the following function:

    f(n) = (PHI^n - CHI^n) / sqrt(5)

    f(n)     = ((PHI^(n - 1) + PHI^(n - 2)) - (CHI^(n - 1) + CHI^(n - 2))) / sqrt(5)
    f(n + 1) = ((PHI^(  n  ) + PHI^(n - 1)) - (CHI^(  n  ) + CHI^(n - 1))) / sqrt(5)
    f(n + 2) = ((PHI^(n + 1) + PHI^(  n  )) - (CHI^(n + 1) + CHI^(  n  ))) / sqrt(5)

Performing substition, we find that

    f(n) + f(n + 1)
    =   (PHI^(n - 1) + PHI^(n - 2) + PHI(  n  ) + PHI(n - 1))
    - (CHI^(n - 1) + CHI^(n - 2) + CHI(  n  ) + CHI(n - 1))
    --------------------------------------------------------
                            sqrt(5)

    f(n) + f(n + 1)
    =   (PHI^(n - 1) + PHI^(n - 2) + PHI(  n  ) + PHI(n - 1))
    - (CHI^(n - 1) + CHI^(n - 2) + CHI(  n  ) + CHI(n - 1))
    --------------------------------------------------------
                            sqrt(5)

    Recalling that PHI^(n - 1) + PHI^(n - 2) = PHI(  n  )
        and that PHI^(  n  ) + PHI^(n - 1) = PHI(n + 1)

    f(n) + f(n + 1)
    =   (PHI^(  n  ) + PHI^(n + 1) - (CHI^(  n  ) + CHI^(n + 1)
    ---------------------------------------------------------
                            sqrt(5)

    >>> f(n) + f(n + 1) = f(n + 2) <<<

Does this function fit the definition of the Fibonacci numbers? Let's check!

f(0) = (PHI^0 - CHI^0) / sqrt(5)
f(0) = 0 / sqrt(5)
f(0) = 0

Mmmm promising :3

f(1) = (PHI^1 - CHI^1) / sqrt(5)
f(1) = (((1 + sqrt(5)) - (1 - sqrt(5)))/2 ) / sqrt(5)
f(1) = (((1 + sqrt(5)) - (1 - sqrt(5)))/2 ) / sqrt(5)
f(1) = (1 - 1 + sqrt(5) + sqrt(5)) /2 ) / sqrt(5)
f(1) = sqrt(5) / sqrt(5)
f(1) = 1

YAY YAY YAY :3

Through induction ----> FWOOSH

So, now we know that f(n) = fib(n).

    fib(n) = (PHI^n - CHI^n) / sqrt(5)

How do we proove the actual proposition?

    fib(n) = round(PHI^n/sqrt(5))


For this, we have to prove that, for any n,

    abs(CHI^n) < (1/2)

hmmmmm....





