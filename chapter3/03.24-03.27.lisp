;;   *Figure 3.22:* A table represented as a headed list.

;;         +---+---+    +---+---+    +---+---+    +---+---+
;;         | * | *-+--->| * | *-+--->| * | *-+--->| * | / |
;;         +-|-+---+    +-|-+---+    +-|-+---+    +-|-+---+
;;           |            |            |            |
;;           V            V            V            V
;;        +---------+   +---+---+   +---+---+   +---+---+
;;        | *table* |   | * | * |   | * | * |   | * | * |
;;        +---------+   +-|-+-|-+   +-|-+-|-+   +-|-+-|-+
;;                        |   |       |   |       |   |
;;                        V   V       V   V       V   V
;;                     +---+ +---+ +---+ +---+ +---+ +---+
;;                     | a | | 1 | | b | | 2 | | c | | 3 |
;;                     +---+ +---+ +---+ +---+ +---+ +---+

     (define (make-table-msg-pass same-key?)
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

;;  Using `make-table', we could implement the `get' and `put'
;;  operations used in section *Note 2-4-3:: for data-directed programming,
;;  as follows:

     (define operation-table (make-table-msg-pass eq?))
     (define get (operation-table 'lookup-proc))
     (define put (operation-table 'insert-proc!))

;;  `Get' takes as arguments two keys, and `put' takes as arguments two
;;  keys and a value.  Both operations access the same local table, which is
;;  encapsulated within the object created by the call to `make-table'.

;;   *Exercise 3.24:* In the table implementations above, the keys are
;;   tested for equality using `equal?' (called by `assoc').  This is
;;   not always the appropriate test.  For instance, we might have a
;;   table with numeric keys in which we don't need an exact match to
;;   the number we're looking up, but only a number within some
;;   tolerance of it.  Design a table constructor `make-table' that
;;   takes as an argument a `same-key?' procedure that will be used to
;;   test "equality" of keys.  `Make-table' should return a `dispatch'
;;   procedure that can be used to access appropriate `lookup' and
;;   `insert!' procedures for a local table.

;; Done! see above

;; Testing:

(define eq-table (make-table-msg-pass eq?)) 

((eq-table 'insert-proc!) 'a 'b 1) 
((eq-table 'insert-proc!) 'a 'c 2) 
((eq-table 'insert-proc!) 'a 2 5) 

((eq-table 'lookup-proc) 'a 'b) 
((eq-table 'lookup-proc) 'a 'c) 
((eq-table 'lookup-proc) 'a 2) 

(define =-table (make-table-msg-pass =)) 
((=-table 'insert-proc!) 'a 'b 1) 

((=-table 'lookup-proc) 'a 'b) 
; ERROR!
;The object a, passed as the first argument to integer-equal?, is not the correct type.

;;   *Exercise 3.25:* Generalizing one- and two-dimensional tables,
;;   show how to implement a table in which values are stored under an
;;   arbitrary number of keys and different values may be stored under
;;   different numbers of keys.  The `lookup' and `insert!' procedures
;;   should take as input a list of keys used to access the table.

(define (is-table? var)
  (if (not (pair? var)) false 
    (eq? (car var) '*table*)))

(define my-table (make-table)) 

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
      false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)

;; At a given step:
;; look for the current key.
;; 0) if this is the LAST key, insert our 'value' as above.
;; otherwise, perform the following:
;; 1) if no record is found, add a NEW TABLE associated
;; with the current key, and recursively insert again
;; using the remaining keys.
;; 2) if a record is found:
;;   a) if that record is a table, recursively insert
;;      with the remaining keys.
;;   b) if that record is not a table, create a NEW TABLE,
;;      replace that record's value, and recursively
;;      insert again with the remaining keys

(define (rec-insert! keys value table)
  (if (null? (cdr keys)) (insert! (car keys) value table) ;; 0
    (let ((record (assoc (car keys) (cdr table)))
          (curr-key (car keys))
          (rest-keys (cdr keys)))
      (cond ((not record) ;; 1
             (let ((new-table (make-table)))
               (begin (insert! curr-key new-table table)
                      (rec-insert! rest-keys value new-table))))
            (else ;;2
             (display record)
             (newline)
             (cond ((is-table? (cdr record)) ;; 2a
                    (display "2a")
                    (rec-insert! rest-keys value (cdr record)))
                   (else ;; 2b
                    (display "2b")
                    (let ((new-table (make-table)))
                      (begin (insert! curr-key new-table table)
                             (rec-insert! rest-keys value new-table)))))
             )))))

(define (make-table)
  (list '*table*))

(define my-table (make-table)) 

(rec-insert! '(test best) 'yay my-table) 
(rec-insert! '(a b) 'bee my-table) 
(rec-insert! '(a c) 'sea my-table) 

my-table 

;; To recursively lookup, at each step:
;; perform a look up in the table using assoc:
;; 0) if assoc returns false, return false
;; otherwise:
;; 1) if this is the final key, return the found record
;;    otherwise:
;; 2) if the record is NOT a table, return false
;;    otherwise:
;; 3) if the record is a table, perform a recursive
;;    lookup on that table using the remaining keys.

(define (rec-lookup keys table)
  (let ((curr-key (car keys))
        (rest-keys (cdr keys)))
    (let ((record (assoc curr-key (cdr table))))
      (if (not record) false ;;0
        (cond ((null? rest-keys) ;; 1
               (if record (cdr record) false))
              ((not (is-table? (cdr record))) false) ;; 2
              (else ;; 3
               (rec-lookup rest-keys (cdr record)))))))) 

my-table 

(rec-lookup '(a) my-table) 

;;   *Exercise 3.26:* To search a table as implemented above, one needs
;;   to scan through the list of records.  This is basically the
;;   unordered list representation of section *Note 2-3-3::.  For large
;;   tables, it may be more efficient to structure the table in a
;;   different manner.  Describe a table implementation where the (key,
;;   value) records are organized using a binary tree, assuming that
;;   keys can be ordered in some way (e.g., numerically or
;;   alphabetically).  (Compare *Note Exercise 2-66:: of *Note Chapter
;;   2::.)

;; The key realization here is that, even with the heterogenous
;; n-trees-of-trees implementation approach above, the logic that
;; we have to change is isolated to two locations: the first is the
;; assoc function (used to look up a given value in a table
;; representation), and the non-recursive insert! function used
;; in two placed in my rec-insert! function.

;;   *Exercise 3.27:* "Memoization" (also called "tabulation") is a
;;   technique that enables a procedure to record, in a local table,
;;   values that have previously been computed.  This technique can
;;   make a vast difference in the performance of a program.  A memoized
;;   procedure maintains a table in which values of previous calls are
;;   stored using as keys the arguments that produced the values.  When
;;   the memoized procedure is asked to compute a value, it first
;;   checks the table to see if the value is already there and, if so,
;;   just returns that value.  Otherwise, it computes the new value in
;;   the ordinary way and stores this in the table.  As an example of
;;   memoization, recall from section *Note 1-2-2:: the exponential
;;   process for computing Fibonacci numbers:

;;        (define (fib n)
;;          (cond ((= n 0) 0)
;;                ((= n 1) 1)
;;                (else (+ (fib (- n 1))
;;                         (fib (- n 2))))))

;;   The memoized version of the same procedure is

;;        (define memo-fib
;;          (memoize (lambda (n)
;;                     (cond ((= n 0) 0)
;;                           ((= n 1) 1)
;;                           (else (+ (memo-fib (- n 1))
;;                                    (memo-fib (- n 2))))))))

;;   where the memoizer is defined as

;;        (define (memoize f)
;;          (let ((table (make-table)))
;;            (lambda (x)
;;              (let ((previously-computed-result (lookup x table)))
;;                (or previously-computed-result
;;                    (let ((result (f x)))
;;                      (insert! x result table)
;;                      result))))))

;;   Draw an environment diagram to analyze the computation of
;;   `(memo-fib 3)'.  Explain why `memo-fib' computes the nth Fibonacci
;;   number in a number of steps proportional to n.  Would the scheme
;;   still work if we had simply defined `memo-fib' to be `(memoize
;;   fib)'?
