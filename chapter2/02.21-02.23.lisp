;;   *Exercise 2.21:* The procedure `square-list' takes a list of
;;   numbers as argument and returns a list of the squares of those
;;   numbers.

        ;;(square-list (list 1 2 3 4))
        ;;(1 4 9 16)

;;   Here are two different definitions of `square-list'.  Complete
;;   both of them by filling in the missing expressions:

;;        (define (square-list items)
;;          (if (null? items)
;;              nil 
;;              (cons <??> <??>)))

;;        (define (square-list items)
;;          (map <??> <??>))

(define nil '()) 

(define (square-list items)
  (if (null? items)
      nil 
    (cons (square (car items))
          (square-list (cdr items)))))


(define (square-list-map items)
  (map square items))

(square-list (list 1 2 3 4))

(square-list-map (list 1 2 3 4))

;;   *Exercise 2.22:* Louis Reasoner tries to rewrite the first
;;   `square-list' procedure of *Note Exercise 2-21:: so that it
;;   evolves an iterative process:

          (define (square-list-iter items)
            (define (iter things answer)
              (if (null? things)
                  answer
                  (iter (cdr things)
                        (cons (square (car things))
                              answer))))
            (iter items nil))

(square-list-iter (list 1 2 3 4))

;;   Unfortunately, defining `square-list' this way produces the answer
;;   list in the reverse order of the one desired.  Why?

;;   The reversal occurs in the step during which we are combining the
;;   partially constructed answer with the current value -
;;   (cons (square (car things)) answer)
;;   Examining this statement, we can see that we are adding the number
;;   to the front of the list! (cons a (b, c)) = (a b c)
;;   Now, consider that we are traversing through the input from
;;   beginning to end - we are placing each item behind the items we
;;   have previously visited, neatly reversing their order.

;;   Louis then tries to fix his bug by interchanging the arguments to
;;   `cons':

          (define (square-list-iter2 items)
            (define (iter things answer)
              (if (null? things)
                  answer
                  (iter (cdr things)
                        (cons answer
                              (square (car things))))))
            (iter items nil))

(square-list-iter2 (list 1 2 3 4))

;;   This doesn't work either.  Explain.

;;   This error is a bit more subtle! Evaluating the expression above
;;   yields the result ((((() . 1) . 4) . 8) . 16) -
;;   While the items are in the correct order (hurray!), they are not
;;   joined together in a list! -or, at least, not the sort of list
;;   that we want (it is worth noting that this structure certainly
;;   could be used as a list!).
;;   
;;   Why is this occurring???
;;
;;   To find out, we take a closer look at the const statement we are
;;   using to build up our answer.
;;
;;   (cons answer (square (car things)))
;;
;;   At first glance, this appears correct! We are joining together
;;   the values we have seen so far with the latest value, with the
;;   older values first and the new values second. So what's the give??
;;   To understand, we should recall what a lisp list actually is!
;;
;;   A lisp list is a chain of pairs, taking the form
;;   '(a . (b . (c . d))) 
;;   The car of each item is the 'value' at that point, and the cdr
;;   is another list!
;;
;;   (cons 'a (cons 'b (cons 'c 'd))) 
;;
;;   Recalling this definition, it becomes clear that the expression
;;   above is building the structure exactly backwards! The car
;;   expression of each pair is a list, while the cdr is the value
;;   at that node. To iterate through such a list, you would recurse
;;   on the car!
;;
;;   With this under our belt, the regrettably complex append
;;   function defined earlier in the chapter suddenly makes a great
;;   deal more sense! It is entirely necessary and exactly what is
;;   needed for this code!

          (define (square-list-iter-good items)
            (define (iter things answer)
              (if (null? things)
                  answer
                  (iter (cdr things)
                        (append
                            answer
                            (list (square (car things)))))))
            (iter items nil))


(square-list-iter-good (list 1 2 3 4)) 

;;   *Exercise 2.23:* The procedure `for-each' is similar to `map'.  It
;;   takes as arguments a procedure and a list of elements.  However,
;;   rather than forming a list of the results, `for-each' just applies
;;   the procedure to each of the elements in turn, from left to right.
;;   The values returned by applying the procedure to the elements are
;;   not used at all--`for-each' is used with procedures that perform
;;   an action, such as printing.  For example,

;;        (for-each (lambda (x) (newline) (display x))
;;                  (list 57 321 88))
;;        57
;;        321
;;        88

;;   The value returned by the call to `for-each' (not illustrated
;;   above) can be something arbitrary, such as true.  Give an
;;   implementation of `for-each'.

(define (for-each lam ls)
  (if (null? ls) #t
    (begin
     (lam (car ls))
     (for-each lam (cdr ls))))) 

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
