
;;   *Exercise 2.3:* Implement a representation for rectangles in a
;;   plane.  (Hint: You may want to make use of *Note Exercise 2-2::.)
;;   In terms of your constructors and selectors, create procedures
;;   that compute the perimeter and the area of a given rectangle.  Now
;;   implement a different representation for rectangles.  Can you
;;   design your system with suitable abstraction barriers, so that the
;;   same perimeter and area procedures will work using either
;;   representation?

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment a b) (cons a b)) 
(define (end-segment s) (cdr s)) 
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (dist a b)
  (let ((x-dist (abs (- (x-point a) (x-point b))))
        (y-dist (abs (- (y-point a) (y-point b)))))
    (sqrt (+ (square x-dist) (square y-dist)))))

(dist (make-point 0 0) (make-point (sqrt 3) (sqrt 6))) 

(min 1 2 3) 
(max 1 2 3) 

(define (point-rectangle a b c d)
  (let ((dist1 (dist a b))
        (dist2 (dist a c))
        (dist3 (dist a d)))
    (let ((height (min dist1 dist2 dist3))
          (width  (- (+ dist1 dist2 dist3)
                     (max dist1 dist2 dist3)
                     (min dist1 dist2 dist3))))
      (lambda (command)
        (let ((my-points (list a b c d)))
          (cond ((eq? command 'height) height)
                ((eq? command 'width)  width)
                ((= 1 1) -1)) 
        ))))) 

(define my-rect (point-rectangle (make-point 0 0)
                                 (make-point 0 1)
                                 (make-point 3 0)
                                 (make-point 3 1)))

(define (perimeter rect)
  (+ (* 2 (rect 'height)) (* 2 (rect 'width)))) 

(define (area rect)
  (* (rect 'height) (rect 'width))) 

(area my-rect) 
(perimeter my-rect) 

(define (other-rectangle width height center rotation)
  (lambda (command)
    (cond ((eq? command 'height) height)
          ((eq? command 'width) width)
          ((= 1 1) -1)))) 

(define my-other-rect (other-rectangle 3 4 (make-point 3 2) 34.5)) 

(area my-other-rect) 
(perimeter my-other-rect) 
