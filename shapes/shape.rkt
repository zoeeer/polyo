#lang racket

(require "points.rkt"
         "../commons/coord.rkt")

(provide (all-defined-out))

;; shapes are stored as a list of points,
;; while a point is a coord (x, y)

(struct shape (points
               [weight #:auto #:mutable]
               [height #:auto #:mutable]
               [width #:auto #:mutable]
               [center #:auto #:mutable]
               )
  #:transparent)

(define (make-shape points)
  (define obj (shape (sort points point>)))
  (set-shape-weight! obj (length points))
  (set-shape-height! obj (points-height points))
  (set-shape-width!  obj (points-width  points))
  (set-shape-center! obj (points-center points))
  obj)

(define (shape= s1 s2)
  (and (= (shape-weight s1) (shape-weight s2))
       (= (shape-height s1) (shape-height s2))
       (= (shape-width s1) (shape-width s2))
       (equal? (shape-points s1) (shape-points s2))))

;; transformations
;; one shape can have at most 8 different symmetric shapes,
;; aka. 7 transformations (reflections and rotations) plus itself

(define (shape-transformations s)
  (let ((s-transposed (shape-transpose s))
        (s-ud-flipped (shape-refl-ud s)))
    (list s-transposed
          s-ud-flipped
          (shape-refl-lr s)
          (shape-rotate-180 s)
          (shape-refl-ud s-transposed) ;; shape-rotate+90
          (shape-transpose s-ud-flipped) ;; shape-rotate-90
          (shape-rotate-180 s-transposed)))) ;; shape-refl-ulrd

;; like above but a little less efficient
;; (define (shape-transformations s)
;;   (map (lambda (op) (op s))
;;        (list shape-refl-ud
;;              shape-refl-lr
;;              shape-transpose
;;              shape-rotate-180
;;              shape-rotate+90
;;              shape-rotate-90
;;              shape-refl-ulrd)))

;; rotations (a one-side shape can have rotations but not reflections)
(define (shape-rotations s)
  (map (lambda (op) (op s))
       (list shape-rotate-180
             shape-rotate+90
             shape-rotate-90)))

;; transformations including itself
(define (shape-transformations* s)
  (cons s (shape-transformations s)))

;; transformations including itself
(define (shape-rotations* s)
  (cons s (shape-rotations s)))

(define (shape-refl-ud s)
  (make-shape
   (map (lambda (p)
          (coord-refl-ud p (coord-y (shape-center s))))
        (shape-points s))))

(define (shape-refl-lr s)
  (make-shape
   (map (lambda (p)
          (coord-refl-lr p (coord-x (shape-center s))))
        (shape-points s))))

(define (shape-transpose s)
  (make-shape
   (map (lambda (p)
          (coord-transpose p))
        (shape-points s))))

(define (shape-rotate-180 s)
  (make-shape
   (map (lambda (p)
          (coord-refl-origin p (shape-center s)))
        (shape-points s))))

(define (shape-rotate+90 s)
  ((compose1 shape-transpose shape-refl-ud) s))

(define (shape-rotate-90 s)
  ((compose1 shape-refl-ud shape-transpose) s))

(define (shape-refl-ulrd s)
  ((compose1 shape-transpose shape-rotate-180) s))

;; remove identical shapes from a list of shapes
(define (remove-identical-shapes lst)
  (if (null? lst)
      lst
      (let ((s (car lst)))
        (cons s (remove* (shape-transformations* s)
                         (remove-identical-shapes (cdr lst)))))))

;; remove duplicate shapes from a list of shapes
;; a shape's identity is determined by each transformation in "transforms"
(define (remove-duplicate-shapes lst [transforms null])
  (cond ((null? lst) lst)
        ((null? transforms)
         (remove-duplicates lst))
        (else
         (let ((s (car lst)))
           (cons s (remove* (transforms s)
                            (remove-duplicate-shapes (cdr lst) transforms)))))))

;; translation of a shape
;; returns a new instance of shape
(define (shape-translation s vec)
  (let* ((points (shape-points s))
         (new-points (points-shift points vec))
         (new-shape (shape new-points)))
    (set-shape-weight! new-shape (shape-weight s))
    (set-shape-height! new-shape (shape-height s))
    (set-shape-width! new-shape (shape-width s))
    (set-shape-center! new-shape (points-center points))
    new-shape))

;; shape->matrix returns a 2d nested list represented by 1's and 0's
;; this implementation depends on that the points in a shape are
;; sorted desendently
(define (shape->matrix s)
  (define width (shape-width s)) ;; could be used frequently by (next c)
  
  (define (next c) ;; next coord
    (if (= (coord-x c) 0)
        (coord width (- (coord-y c) 1))
        (coord (- (coord-x c) 1) (coord-y c))))

  (define (iter lst current-coord points)
    (if (< (coord-y current-coord) 0) ;; end of loop
        lst
        (let ((elem (if (or (null? points)
                            (point> current-coord (car points)))
                        0
                        1)))
          (iter (if (= (coord-x current-coord) width)
                    (cons (list elem) lst)
                    (cons (cons elem (car lst)) (cdr lst)))
                (next current-coord)
                (if (zero? elem) points (cdr points))))))
  (iter null
        (coord (shape-width s) (shape-height s))
        (shape-points s)))
