 ;;  *Exercise 2.40:* Define a procedure `unique-pairs' that, given an
 ;;  integer n, generates the sequence of pairs (i,j) with 1 <= j< i <=
 ;;  n.  Use `unique-pairs' to simplify the definition of
 ;;  `prime-sum-pairs' given above.

;;============================
;; Some boiler plate stuff

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

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

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval beg end)
  (if (> beg end)
      nil
    (cons beg (enumerate-interval (+ 1 beg) end))))

(define (enumerate-interval-2 beg end)
  (define (iter result beg curr)
    (if (< curr beg) result
      (iter (cons curr result) beg (- curr 1))))
  (iter nil beg end)) 

;;============================

(define (unique-pairs n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

;; Or,

(define (unique-pairs n)
  (flatmap (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (prime-sum-pairs-2 n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))


 ;;  *Exercise 2.41:* Write a procedure to find all ordered triples of
 ;;  distinct positive integers i, j, and k less than or equal to a
 ;;  given integer n that sum to a given integer s.

(define (unique-pairs n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (unique-triples n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (cons i j))
                          (unique-pairs (- i 1))))
                   (enumerate-interval 1 n))))

;; Aha! A pattern. How might we generalize this? (see end)

(define (unique-triples-2 n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; Expanded a bit, for a clearer structure

(define (unique-quadruples n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (cons i j))
                          (unique-triples (- i 1))))
                   (enumerate-interval 1 n))))

(define (unique-tuples n depth)
  (if (= depth 2)
      (unique-pairs n)
    (accumulate append
                nil
                (map (lambda (i)
                       (map (lambda (j) (cons i j))
                            (unique-tuples (- i 1) (- depth 1))))
                     (enumerate-interval 1 n)))))

(unique-pairs 4) 

(unique-triples 5) 

(unique-tuples 5 3)

(unique-tuples 3 2)

(unique-quadruples 5) 

(unique-tuples 5 4) 

 ;;  *Figure 2.8:* A solution to the eight-queens puzzle.

 ;;       +---+---+---+---+---+---+---+---+
 ;;       |   |   |   |   |   | Q |   |   |
 ;;       +---+---+---+---+---+---+---+---+
 ;;       |   |   | Q |   |   |   |   |   |
 ;;       +---+---+---+---+---+---+---+---+
 ;;       | Q |   |   |   |   |   |   |   |
 ;;       +---+---+---+---+---+---+---+---+
 ;;       |   |   |   |   |   |   | Q |   |
 ;;       +---+---+---+---+---+---+---+---+
 ;;       |   |   |   |   | Q |   |   |   |
 ;;       +---+---+---+---+---+---+---+---+
 ;;       |   |   |   |   |   |   |   | Q |
 ;;       +---+---+---+---+---+---+---+---+
 ;;       |   | Q |   |   |   |   |   |   |
 ;;       +---+---+---+---+---+---+---+---+
 ;;       |   |   |   | Q |   |   |   |   |
 ;;       +---+---+---+---+---+---+---+---+

 ;;  *Exercise 2.42:* The "eight-queens puzzle" asks how to place eight
 ;;  queens on a chessboard so that no queen is in check from any other
 ;;  (i.e., no two queens are in the same row, column, or diagonal).
 ;;  One possible solution is shown in *Note Figure 2-8::.  One way to
 ;;  solve the puzzle is to work across the board, placing a queen in
 ;;  each column.  Once we have placed k - 1 queens, we must place the
 ;;  kth queen in a position where it does not check any of the queens
 ;;  already on the board.  We can formulate this approach recursively:
 ;;  Assume that we have already generated the sequence of all possible
 ;;  ways to place k - 1 queens in the first k - 1 columns of the
 ;;  board.  For each of these ways, generate an extended set of
 ;;  positions by placing a queen in each row of the kth column.  Now
 ;;  filter these, keeping only the positions for which the queen in
 ;;  the kth column is safe with respect to the other queens.  This
 ;;  produces the sequence of all ways to place k queens in the first k
 ;;  columns.  By continuing this process, we will produce not only one
 ;;  solution, but all solutions to the puzzle.

 ;;  We implement this solution as a procedure `queens', which returns a
 ;;  sequence of all solutions to the problem of placing n queens on an
 ;;  n*n chessboard.  `Queens' has an internal procedure `queen-cols'
 ;;  that returns the sequence of all ways to place queens in the first
 ;;  k columns of the board.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
      (filter
       (lambda (positions) (safe? k positions))
       (flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))
        (queen-cols (- k 1))))))
  (queen-cols board-size))

 ;;  In this procedure `rest-of-queens' is a way to place k - 1 queens
 ;;  in the first k - 1 columns, and `new-row' is a proposed row in
 ;;  which to place the queen for the kth column.  Complete the program
 ;;  by implementing the representation for sets of board positions,
 ;;  including the procedure `adjoin-position', which adjoins a new
 ;;  row-column position to a set of positions, and `empty-board',
 ;;  which represents an empty set of positions.  You must also write
 ;;  the procedure `safe?', which determines for a set of positions,
 ;;  whether the queen in the kth column is safe with respect to the
 ;;  others.  (Note that we need only check whether the new queen is
 ;;  safe--the other queens are already guaranteed safe with respect to
 ;;  each other.)

(define (make-position row col)
  (cons row col)) 

(define (row position)
  (car position))

(define (col position)
  (cdr position))

;; -------------
;;              |
;;              v
(define nil '())

(define empty-board nil)

;; -------------
;;              |
;;              v
(define (adjoin-position new-row k positions)
  (append positions (list (make-position new-row k))))


(define (on-col-with? pos1 pos2)
  (= (col pos1) (col pos2))) 

(define (on-row-with? pos1 pos2)
  (= (row pos1) (row pos2))) 

;;  (define (on-diag-with? pos1 pos2)
;;  (= (- (row pos1) (row pos2))
;;      (- (col pos1) (col pos2))))

(define (on-diag-with? pos1 pos2)
  (= (abs (- (row pos1) (row pos2)))
     (abs (- (col pos1) (col pos2)))))

(define (safe-wrt? pos1 pos2)
  (and (not (on-col-with? pos1 pos2))
       (not (on-row-with? pos1 pos2))
       (not (on-diag-with? pos1 pos2)))) 

(define (seek-pos k positions)
  (cond ((null? positions) nil)
        ((= k 0) (car positions))
        (else (seek-pos (- k 1) (cdr positions)))))

(define (filter-pos k positions)
  (if (= k 0) (cdr positions)
    (cons (car positions) (filter-pos (- k 1) (cdr positions)))))

;; 0123 
;;0  x
;;1x      
;;2   x
;;3 x

(define tp
  (list (make-position 1 0)
        (make-position 3 1)
        (make-position 0 2)
        (make-position 2 3)))

(define tp2
  (list (make-position 1 0)
        (make-position 3 1)
        (make-position 0 2)
        (make-position 3 3)))

(safe? 1 tp2)

;; -------------
;;              |
;;              v
(define (safe? 1-indexed positions)
  (define k (- 1-indexed 1))
  (define kth-pos (seek-pos k positions))
  (define (helper k positions)
    (if (null? positions) #t
      (and (safe-wrt? kth-pos (car positions)) (helper k (cdr positions)))))
  (helper k (filter-pos k positions)))

(safe? 2 (car (queens 2)))
(safe-wrt?
 (seek-pos 0 (car (queens 2))) 
 (seek-pos 1 (car (queens 2))))


;; 12
;;1 x
;;2x

(length (queens 9)) 

(define (queened? pos board)
  (not (null? (find (lambda(x) (and (= (row x) (row pos))
                                    (= (col x) (col pos))))
                    board))))
                           

 ;;  *Exercise 2.43:* Louis Reasoner is having a terrible time doing
 ;;  *Note Exercise 2-42::.  His `queens' procedure seems to work, but
 ;;  it runs extremely slowly.  (Louis never does manage to wait long
 ;;  enough for it to solve even the 6*6 case.)  When Louis asks Eva Lu
 ;;  Ator for help, she points out that he has interchanged the order
 ;;  of the nested mappings in the `flatmap', writing it as

 ;;       (flatmap
 ;;        (lambda (new-row)
 ;;          (map (lambda (rest-of-queens)
 ;;                 (adjoin-position new-row k rest-of-queens))
 ;;               (queen-cols (- k 1))))
 ;;        (enumerate-interval 1 board-size))

 ;;  Explain why this interchange makes the program run slowly.
 ;;  Estimate how long it will take Louis's program to solve the
 ;;  eight-queens puzzle, assuming that the program in *Note Exercise
 ;;  2-42:: solves the puzzle in time T.

(define (option-1 k)
  (flatmap
   (lambda (new-row)
     (map (lambda (rest-of-queens)
            (adjoin-position new-row k rest-of-queens))
          (queen-cols (- k 1))))
   (enumerate-interval 1 board-size)))

(define (option-2 k)
  (flatmap
   (lambda (rest-of-queens)
     (map (lambda (new-row)
            (adjoin-position new-row k rest-of-queens))
          (enumerate-interval 1 board-size)))
   (queen-cols (- k 1))))

;; The original solution takes the following approach:
;; for each way to place k - 1 queens, we generate
;; board-size new options -> that is, we call queen-cols
;; k - 1 times, and at reach round, create board-size-times-
;; the-current-number-of-solutions solutions, that is,
;; board-size ^ board-size. It is a linear recursion,
;; performing enumerate-interval board-size number of times.

;; T

;; Louis' solution, on the other hand, generates a tree
;; recursion with a branching factor of board-size. In each
;; branch, enumerate-interval is called once - but, the
;; branching recursion means that, in the end, it is called
;; board-size^board-size number of times!! Which is, alas!,
;; a vastly larger order of growth.

;; T^T
