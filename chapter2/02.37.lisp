 ;; Exercise 2.37
 ;; .............

 ;; Suppose we represent vectors v = (v_i) as sequences of numbers, and
 ;; matrices m = (m_(ij)) as sequences of vectors (the rows of the matrix).
 ;; For example, the matrix

 ;;     +-         -+
 ;;     |  1 2 3 4  |
 ;;     |  4 5 6 6  |
 ;;     |  6 7 8 9  |
 ;;     +-         -+

 ;; is represented as the sequence `((1 2 3 4) (4 5 6 6) (6 7 8 9))'.  With
 ;; this representation, we can use sequence operations to concisely
 ;; express the basic matrix and vector operations.  These operations
 ;; (which are described in any book on matrix algebra) are the following:

 ;;                                             __
 ;;     (dot-product v w)      returns the sum >_i v_i w_i

 ;;     (matrix-*-vector m v)  returns the vector t,
 ;;                                         __
 ;;                             where t_i = >_j m_(ij) v_j

 ;;     (matrix-*-matrix m n)  returns the matrix p,
 ;;                                         __
 ;;                             where p_(ij) = >_k m_(ik) n_(kj)

 ;;     (transpose m)          returns the matrix n,
 ;;                             where n_(ij) = m_(ji)

 ;; We can define the dot product as(4)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


 ;; Fill in the missing expressions in the following procedures for
 ;; computing the other matrix operations.  (The procedure `accumulate-n'
 ;; is defined in *Note Exercise 2-36::.)

(define matrix-a '((54 21)
                   (55 18)
                   (56 27)))

(define vector-b '(1 2))

(dot-product vector-b vector-b) 
;; 5

 ;;     (define (matrix-*-vector m v)
 ;;     (map <??> m))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))  m))

(matrix-*-vector matrix-a vector-b) 
;; (96 91 110)

 ;;     (define (transpose mat)
 ;;     (accumulate-n <??> <??> mat))

(define nil '()) 

(define (transpose mat)
  (accumulate-n cons nil mat)) 

(define matrix-b '((1 2 3)
                   (4 5 6)))

(transpose matrix-b) 
;; '((1 4) (2 5) (3 6))

 ;;     (define (matrix-*-matrix m n)
 ;;     (let ((cols (transpose n)))
 ;;         (map <??> m)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define matrix-c '((1 5)
                   (2 2)))

(matrix-*-matrix matrix-a matrix-c) 
;; '((96 91 110) (312 311 334))

 ;;     *Exercise 2.38:* The `accumulate' procedure is also known as
 ;;     `fold-right', because it combines the first element of the
 ;;     sequence with the result of combining all the elements to the
 ;;     right.  There is also a `fold-left', which is similar to
 ;;     `fold-right', except that it combines elements working in the
 ;;     opposite direction:

         (define (fold-left op initial sequence)
             (define (iter result rest)
             (if (null? rest)
                 result
                 (iter (op result (car rest))
                         (cdr rest))))
             (iter initial sequence))

 ;;     What are the values of

        (fold-right / 1 (list 1 2 3))

        (fold-left / 1 (list 1 2 3))

        (fold-right list nil (list 1 2 3))

        (fold-left list nil (list 1 2 3))

 ;;     Give a property that `op' should satisfy to guarantee that
 ;;     `fold-right' and `fold-left' will produce the same values for any
 ;;     sequence.

;; It should be... dun dun dun... Associative, I should think!
;; Maybe one could come up with a more restrictive property,
;; though... Where order does matter, but the direction of
;; traversal doesn't??? A curious thought! Though nothing with
;; that particular property comes to mind... (Benjamin Button
;; functions?)

;;   *Exercise 2.39:* Complete the following definitions of `reverse'
;;   (*Note Exercise 2-18::) in terms of `fold-right' and `fold-left'
;;   from *Note Exercise 2-38:::

;;        (define (reverse sequence)
;;          (fold-right (lambda (x y) <??>) nil sequence))

;;        (define (reverse sequence)
;;          (fold-left (lambda (x y) <??>) nil sequence))

(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x)) ) nil sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse-r '(1 2 3)) 

(reverse-l '(1 2 3)) 
