(define else #t) 

;;   *Exercise 1.14:* Draw the tree illustrating the process generated
;;   by the `count-change' procedure of section *Note 1-2-2:: in making
;;   change for 11 cents.  What are the orders of growth of the space
;;   and number of steps used by this process as the amount to be
;;   changed increases?

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
    (count-change 11) 

;;
;;                          cc 11 5                             
;;                        /         \                           
;;                cc 11 4            cc -39 5                   
;;              /         \                                     
;;         cc 11 3      cc -14 4                                
;;        /       \                                             
;;    cc 11 2       -------------------------------- cc 1 3          
;;    |      \                                      /       \         
;; cc 11 1   cc 10 2                              cc 1 2  cc -9 3      
;;    |        |     \                             |     \              
;; cc 11 0   cc 10 1   -- cc 5 2                  cc 1 1  - cc 1 1     
;;             |             |   \                 |     \         \
;;           cc 10 0      cc 5 1   - cc 0 2       cc 1 0  - cc 0 1  - cc 0 1
;;                         /     \                             
;;                       cc 5 0    - cc 4 1               
;;                                    |      \
;;                                   cc 4 0    - cc 3 1
;;                                               |     \
;;                                            cc 3 0     - cc 2 1         
;;                                                          |     \
;;                                                       cc 2 0     - cc 1 1
;;                                                                     |    \
;;                                                                  cc 1 0    - cc 0 1 
;;                                                                            
;;                                                                             
           
;; Each 'branching' of the tree can be understood as having two parts:
;; 1) a 'vertical' chain about
;;     (/ amount (first-denomination kinds-of-coins))
;; long, and 
;; 2) an additional branching proportional to kinds-of-coins
;;
;; so, roughly, the tree appears to have a complexity proportional to:
;;     (exp (/ amount (first-denomination kinds-of-coins)) kinds-of-coins)
;;
;; Aka, theta(amount^kinds-of-coins)
;;
;; TODO: Return to this and try to figure out an actual proof!
;;       Or, find an actual proof online and understand it.


;;   *Exercise 1.15:* The sine of an angle (specified in radians) can
;;   be computed by making use of the approximation `sin' xapprox x if
;;   x is sufficiently small, and the trigonometric identity

;;                       x             x
;;        sin x = 3 sin --- - 4 sin^3 ---
;;                       3             3

;;   to reduce the size of the argument of `sin'.  (For purposes of this
;;   exercise an angle is considered "sufficiently small" if its
;;   magnitude is not greater than 0.1 radians.) These ideas are
;;   incorporated in the following procedures:

    (define (cube x) (* x x x))

    (define (p x) (- (* 3 x) (* 4 (cube x))))

    (define (sine angle)
        (if (not (> (abs angle) 0.1))
            angle
            (p (sine (/ angle 3.0)))))

;;     a. How many times is the procedure `p' applied when `(sine
;;        12.15)' is evaluated?

;;     b. What is the order of growth in space and number of steps (as
;;        a function of a) used by the process generated by the `sine'
;;        procedure when `(sine a)' is evaluated?

    (define (sine_count angle count)
        (if (not (> (abs angle) 0.1))
            count
            (sine_count (/ angle 3.0) (+ 1 count))))

    (sine 12.15)
    (sine_count 12.15 0) 
    (sine_count 0.1 0) 
    (sine_count 0.3 0) 
    (sine_count 0.9 0) 
    (sine_count 1.0 0) 
    (sine_count 2.7 0) 
    (sine_count 2.8 0) 

;; a) 5 times
;; b) theta(log_3(n))

;;   *Exercise 1.16:* Design a procedure that evolves an iterative
;;   exponentiation process that uses successive squaring and uses a
;;   logarithmic number of steps, as does `fast-expt'.  (Hint: Using the
;;   observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along with the
;;   exponent n and the base b, an additional state variable a, and
;;   define the state transformation in such a way that the product a
;;   b^n is unchanged from state to state.  At the beginning of the
;;   process a is taken to be 1, and the answer is given by the value
;;   of a at the end of the process.  In general, the technique of
;;   defining an "invariant quantity" that remains unchanged from state
;;   to state is a powerful way to think about the design of iterative
;;   algorithms.)

;; for comparison

    (define (fast-expt b n)
      (cond ((= n 0) 1)
            ((even? n) (square (fast-expt b (/ n 2))))
            (else (* b (fast-expt b (- n 1))))))

;; note that (b^(n/2))^2 = (b^2)^(n/2)

    (define (even? n)
    (= (remainder n 2) 0))

    (define (iter-fast-expt b n)
      (iter-fast-expt-helper b n 1))

    (define (iter-fast-expt-helper b n a)
      (cond ((= n 0) a)
            ((even? n) (iter-fast-expt-helper (* b b) (/ n 2) a))
            (else (iter-fast-expt-helper b
                                         (- n 1)
                                         (* b a)))))

    (iter-fast-expt 2 3) 

    (iter-fast-expt 3 4) 

    (fast-expt 3 4) 

    (iter-fast-expt 2 1) 

;;   *Exercise 1.17:* The exponentiation algorithms in this section are
;;   based on performing exponentiation by means of repeated
;;   multiplication.  In a similar way, one can perform integer
;;   multiplication by means of repeated addition.  The following
;;   multiplication procedure (in which it is assumed that our language
;;   can only add, not multiply) is analogous to the `expt' procedure:

;;        (define (* a b)
;;          (if (= b 0)
;;              0
;;              (+ a (* a (- b 1)))))

;;   This algorithm takes a number of steps that is linear in `b'.  Now
;;   suppose we include, together with addition, operations `double',
;;   which doubles an integer, and `halve', which divides an (even)
;;   integer by 2.  Using these, design a multiplication procedure
;;   analogous to `fast-expt' that uses a logarithmic number of steps.

    (define (even? n)
    (= (remainder n 2) 0))

    (define (double a) (* a 2)) 
    (define (halve a) (/ a 2)) 

    (define (fast-* b n)
    (cond ((= n 0) 0)
            ((even? n) (double (fast-* b (halve n))))
            (else (+ b (fast-* b (- n 1))))))

    (fast-* 2 3)
    (fast-* 3 5)
    (fast-* 9 4) 
    (fast-* 4 9) 
    (fast-* 2 7)

;;   *Exercise 1.18:* Using the results of *Note Exercise 1-16:: and
;;   *Note Exercise 1-17::, devise a procedure that generates an
;;   iterative process for multiplying two integers in terms of adding,
;;   doubling, and halving and uses a logarithmic number of steps.(4)

    (define (iter-fast-* a b)
    (iter-fast-*-helper a b 0))

    (define (iter-fast-*-helper a b sum)
    (cond ((= b 0) sum)
            ((even? b) (iter-fast-*-helper (double a) (halve b) sum))
            (else (iter-fast-*-helper a (- b 1) (+ sum a)))))

    (iter-fast-* 2 3) 
    (iter-fast-* 3 5) 
    (iter-fast-* 4 7) 
    (iter-fast-* 7 4) 
    (iter-fast-* 5 3) 
    (iter-fast-* 9 7) 
    (iter-fast-* 33 58) 

;;   *Exercise 1.19:* There is a clever algorithm for computing the
;;   Fibonacci numbers in a logarithmic number of steps.  Recall the
;;   transformation of the state variables a and b in the `fib-iter'
;;   process of section *Note 1-2-2::: a <- a + b and b <- a.  Call
;;   this transformation T, and observe that applying T over and over
;;   again n times, starting with 1 and 0, produces the pair _Fib_(n +
;;   1) and _Fib_(n).  In other words, the Fibonacci numbers are
;;   produced by applying T^n, the nth power of the transformation T,
;;   starting with the pair (1,0).  Now consider T to be the special

(gcd 4 (remainder 6 4))

;; ----------

(gcd 4 2)

;; ----------

(if (= 2 0)
    4
  (gcd 2 (remainder 4 2)))

;; ----------

(gcd 2 (remainder 4 2))

;; ----------

(gcd 2 0)

;; ----------

(if (= 0 0)
    2
  (gcd 2 (remainder 2 0)))

;; TOTAL : 4
;; wow...

;; ---------------
;; 1-2-5

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Recall:

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;;        .                                      
;;       |  0    ->   1                                     
;; exp --   even -> (sqr (expmod(base, exp/2, m)))  % m
;;       |  odd  -> (m * (expmod(base, exp-1, m))) % m
;;        .                                      

;; Note: this works due to the following equivalence:
;; (remainder (* x y) m) == (remainder (* (remainder x m) (remainder y m)) m)
;; For a good explanation of congruences, I would recommend this handout:
;; http://www.math.cmu.edu/~jmackey/151_128/congruences.pdf
;; For me, the key to understanding congruences was thinking about LEFTOVER BITS!

;; Why is the fact that a is relatively prime to b THE SAME as saying that
;; a has a multiplicative inverse modulo b???

;; Think of it this way: if a is ONE away from b, it must be relatively prime to it!
;; If there were any divisors other than one that they shared, you could add this number
;; to a and get b - and, in this case, that is clearly impossible.

;; If a has a multiplicative inverse module b, it must mean there exists some x and
;; y such that x*a + y*b = 1. Consider also that divisors(x*a) is a superset of
;; divisors(a), and similarly for y*b and b. 

;; Bezout's identity comes into play here... TODO - explain, link to proof.

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 31 4) 


;;   *Exercise 1.21:* Use the `smallest-divisor' procedure to find the
;;   smallest divisor of each of the following numbers: 199, 1999,
;;   19999.

(smallest-divisor 199) 
(smallest-divisor 1999) 
(smallest-divisor 19999) 

;;   *Exercise 1.22:* Most Lisp implementations include a primitive
;;   called `runtime' that returns an integer that specifies the amount
;;   of time the system has been running (measured, for example, in
;;   microseconds).  The following `timed-prime-test' procedure, when
;;   called with an integer n, prints n and checks to see if n is
;;   prime.  If n is prime, the procedure prints three asterisks
;;   followed by the amount of time used in performing the test.


         (define (timed-prime-test n)
            (timed-prime-test-generic n prime?))

         (define (timed-prime-test-generic n prime-tester)
            (start-prime-test n (runtime) prime-tester))

         (timed-prime-test 5) 
         (start-prime-test 5 (runtime) prime?) 

         (define (start-prime-test n start-time prime-tester)
           (if (prime-tester n) (report-prime n (- (runtime) start-time))
               #f))  

         (define (report-prime n elapsed-time)
            (newline)
            (display n)
            (display " *** ")
            (display elapsed-time)
            #t) 


;;   Using this procedure, write a procedure `search-for-primes' that
;;   checks the primality of consecutive odd integers in a specified
;;   range.  Use your procedure to find the three smallest primes
;;   larger than 1000; larger than 10,000; larger than 100,000; larger
;;   than 1,000,000.  Note the time needed to test each prime.  Since
;;   the testing algorithm has order of growth of [theta](_[sqrt]_(n)),
;;   you should expect that testing for primes around 10,000 should
;;   take about _[sqrt]_(10) times as long as testing for primes around
;;   1000.  Do your timing data bear this out?  How well do the data
;;   for 100,000 and 1,000,000 support the _[sqrt]_(n) prediction?  Is
;;   your result compatible with the notion that programs on your
;;   machine run in time proportional to the number of steps required
;;   for the computation?

(define (search-for-primes min max)
  (search 0 min max))

(define (search num-found curr max)
  (cond ((or (= num-found 3) (> curr max)) ())
        ((timed-prime-test curr) (search (+ num-found 1) (+ curr 1) max))
        (else (search num-found (+ curr 1) max))))

(search-for-primes 0 10) 

;;   *Exercise 1.23:* The `smallest-divisor' procedure shown at the
;;   start of this section does lots of needless testing: After it
;;   checks to see if the number is divisible by 2 there is no point in
;;   checking to see if it is divisible by any larger even numbers.
;;   This suggests that the values used for `test-divisor' should not
;;   be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, ....  To
;;   implement this change, define a procedure `next' that returns 3 if
;;   its input is equal to 2 and otherwise returns its input plus 2.
;;   Modify the `smallest-divisor' procedure to use `(next
;;   test-divisor)' instead of `(+ test-divisor 1)'.  With
;;   `timed-prime-test' incorporating this modified version of
;;   `smallest-divisor', run the test for each of the 12 primes found in
;;   *Note Exercise 1-22::.  Since this modification halves the number
;;   of test steps, you should expect it to run about twice as fast.
;;   Is this expectation confirmed?  If not, what is the observed ratio
;;   of the speeds of the two algorithms, and how do you explain the
;;   fact that it is different from 2?

(define (next n)
  (if (= n 2) 3 (+ 2 n)))

(define (faster-smallest-divisor n)
  (faster-find-divisor n 2))

(define (faster-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (faster-prime? n)
  (= n (faster-smallest-divisor n)))

(define (repeat n i times func)
  (cond ((> i times) ())
        (else (func n) (repeat n (+ i 1) times func))))

(define (timed-prime-test-faster n)
  (timed-prime-test-generic n faster-prime?))

(define (fast-prime?-32 n)
  (fast-prime? n 32))

(define (timed-prime-test-fastest n)
  (timed-prime-test-generic n fast-prime?-32)) ;; oho! so fast

(define (avg-slo n times)
  (repeat n 0 times timed-prime-test))

(define (avg-faster n times)
  (repeat n 0 times timed-prime-test-faster))

(define (avg-fastest n times)
  (repeat n 0 times timed-prime-test-fastest))

;;   *Exercise 1.25:* Alyssa P. Hacker complains that we went to a lot
;;   of extra work in writing `expmod'.  After all, she says, since we
;;   already know how to compute exponentials, we could have simply
;;   written

;;        (define (expmod base exp m)
;;          (remainder (fast-expt base exp) m))

;;   Is she correct?  Would this procedure serve as well for our fast
;;   prime tester?  Explain.

;;   Nope nope! You see, this procedure, in contrast to the one
;;   implemented above, very quickly generates large numbers!
;;   Large numbers have some downsides... One is the possibility
;;   of overflow, which threatens correctness itself! But also, and
;;   more relevantly, perhaps, this being a "fast prime tester",
;;   it takes significantly longer amounts of time to perform calculations
;;   on larger numbers, compared to small.


;;   *Exercise 1.26:* Louis Reasoner is having great difficulty doing
;;   *Note Exercise 1-24::.  His `fast-prime?' test seems to run more
;;   slowly than his `prime?' test.  Louis calls his friend Eva Lu Ator
;;   over to help.  When they examine Louis's code, they find that he
;;   has rewritten the `expmod' procedure to use an explicit
;;   multiplication, rather than calling `square':

;;        (define (expmod base exp m)
;;          (cond ((= exp 0) 1)
;;                ((even? exp)
;;                 (remainder (* (expmod base (/ exp 2) m)
;;                               (expmod base (/ exp 2) m))
;;                            m))
;;                (else
;;                 (remainder (* base (expmod base (- exp 1) m))
;;                            m))))

;;   "I don't see what difference that could make," says Louis.  "I
;;   do."  says Eva.  "By writing the procedure like that, you have
;;   transformed the [theta](`log' n) process into a [theta](n)
;;   process."  Explain.

;;  Eva is right - examining the code, we see that, at the squaring
;;  step, we are branching in two at each level of the recursive stack!
;;  This means that, roughly, the number of operations is increased by
;;  2 ^ depth of recursion! Since we have established that this is a
;;  [theta](`log' n) process, we can express the order of growth of
;;  this function as (^ 2 (log_2 n)), which simplifies to n


;;   *Exercise 1.27:* Demonstrate that the Carmichael numbers listed in
;;   *Note Footnote 1-47:: really do fool the Fermat test.  That is,
;;   write a procedure that takes an integer n and tests whether a^n is
;;   congruent to a modulo n for every a<n, and try your procedure on
;;   the given Carmichael numbers.

;;  (4) [Footnote 1.47] Numbers that fool the Fermat test are called "Carmichael
;;  numbers", and little is known about them other than that they are
;;  extremely rare.  There are 255 Carmichael numbers below 100,000,000.
;;  The smallest few are 561, 1105, 1729, 2465, 2821, and 6601.  In testing
;;  primality of very large numbers chosen at random, the chance of
;;  stumbling upon a value that fools the Fermat test is less than the
;;  chance that cosmic radiation will cause the computer to make an error
;;  in carrying out a "correct" algorithm.  Considering an algorithm to be
;;  inadequate for the first reason but not for the second illustrates the
;;  difference between mathematics and engineering.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (check-modulo n a)
    (= (expmod a n n) a))

(define (check-modulos n)
  (define (check-modulos-iter n counter)
    (cond ((= 1 counter) #t)
          ((not (check-modulo n counter)) #f)
          ((= 1 1) (check-modulos-iter n (- counter 1))) 
          )
    )
  (check-modulos-iter n (- n 1)))

(define (carmichael i)
  (cond ((= 0 i) 561)
        ((= 1 i) 1105)
        ((= 2 i) 1729)
        ((= 3 i) 2465)
        ((= 4 i) 2821)
        ((= 5 i) 6601)))

(check-modulos (carmichael 3)) 
(prime? (carmichael 3)) 

;;   *Exercise 1.28:* One variant of the Fermat test that cannot be
;;   fooled is called the "Miller-Rabin test" (Miller 1976; Rabin
;;   1980).  This starts from an alternate form of Fermat's Little
;;   Theorem, which states that if n is a prime number and a is any
;;   positive integer less than n, then a raised to the (n - 1)st power
;;   is congruent to 1 modulo n.  To test the primality of a number n
;;   by the Miller-Rabin test, we pick a random number a<n and raise a
;;   to the (n - 1)st power modulo n using the `expmod' procedure.
;;   However, whenever we perform the squaring step in `expmod', we
;;   check to see if we have discovered a "nontrivial square root of 1
;;   modulo n," that is, a number not equal to 1 or n - 1 whose square
;;   is equal to 1 modulo n.  It is possible to prove that if such a
;;   nontrivial square root of 1 exists, then n is not prime.  It is
;;   also possible to prove that if n is an odd number that is not
;;   prime, then, for at least half the numbers a<n, computing a^(n-1)
;;   in this way will reveal a nontrivial square root of 1 modulo n.
;;   (This is why the Miller-Rabin test cannot be fooled.)  Modify the
;;   `expmod' procedure to signal if it discovers a nontrivial square
;;   root of 1, and use this to implement the Miller-Rabin test with a
;;   procedure analogous to `fermat-test'.  Check your procedure by
;;   testing various known primes and non-primes.  Hint: One convenient
;;   way to make `expmod' signal is to have it return 0.


;;   ----> A number that is not equal to 1 or n - 1 whose square is
;;         congruent to 1, modulo n

;;   n IN PRIMES
;;   a < n, a IN POS_INTS
;;   a^(n - 1) IS CONGRUENT to 1, MODULO n

(define (non-trivial-sqrt? c n)
  (if (or (= c 1) (= c (- n 1))) #f
    (let ((result (= (remainder (square c) n) 1)))
;;    (display "c: ")
;;    (display c)
;;    (newline)
;;    (display "result: ")
;;    (display result)
;;    (newline)
      result
    )))

(non-trivial-sqrt? 4 13) 

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((x (expmod2 base (/ exp 2) m)))
           (if (non-trivial-sqrt? x m) 0 (remainder (square x) m))))
        (else
         (remainder (* base (expmod2 base (- exp 1) m))
                    m))))

(define (miller-rabin-test n) 
  (define (try-it a)
    (display "trying a: ")
    (display a)
    (newline)
    (let ((x (expmod2 a n n)))
      (display "x: ")
      (display x)
      (newline)
      (not (or (= x 0) (= x 1)))))
  (try-it (+ 2 (random (- n 2)))))

(define (miller-rabin? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (miller-rabin? n (- times 1)))
        (else #f)))

(prime? 13) 

(carmichael 3) 

(miller-rabin? (carmichael 3) 10000) 

(miller-rabin? 13 100) 



