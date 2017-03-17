#lang racket/base

(require "../commons/numerics.rkt"
         "../commons/coord.rkt")

(provide (all-defined-out))

;; shapes are stored as a list of points,
;; while a point is a coord (x, y)

;; functions for points
(define (points-range points)
  (list (points-x-range points)
        (points-y-range points)))

(define (points-x-range points)
  (range-of (map coord-x points)))

(define (points-y-range points)
  (range-of (map coord-y points)))

(define (points-height points)
  (span (map coord-y points)))

(define (points-width points)
  (span (map coord-x points)))

(define (points-center points)
  (let ((x-range (points-x-range points))
        (y-range (points-y-range points)))
    (coord
     (/ (+ (range-min x-range) (range-max x-range)) 2)
     (/ (+ (range-min y-range) (range-max y-range)) 2))))

;;;; points comparison ;;;;
(define (point> p1 p2)
  (or (> (coord-y p1) (coord-y p2))
      (and (= (coord-y p1) (coord-y p2))
           (> (coord-x p1) (coord-x p2)))))
;; (define (point> p1 p2 . points)
;;   (and (apply > (map coord-y (append (list p1 p2) points)))
;;        (apply > (map coord-x (append (list p1 p2) points)))))

(define (point= p1 p2 . points)
  (and (apply = (map coord-y (append (list p1 p2) points)))
       (apply = (map coord-x (append (list p1 p2) points)))))

(define (point< p1 p2)
  (or (< (coord-y p1) (coord-y p2))
      (and (= (coord-y p1) (coord-y p2))
           (< (coord-x p1) (coord-x p2)))))
;; (define (point< p1 p2 . points)
;;   (and (apply < (map coord-y (append (list p1 p2) points)))
;;        (apply < (map coord-x (append (list p1 p2) points)))))

;; sorted points with desendent order
(define (adjoin-points p l)
  (cond ((null? l) (list p))
        ((point> p (car l))
         (cons p l))
        ((point= p (car l)) l) ;; p is already in l, so return l itself
        (else
         (cons (car l) (adjoin-points p (cdr l))))))

;; translate a set of points
(define (points-shift points vec)
  (map (lambda (p) (coord-shift p vec)) points))

;; this does not work for immutable list of points
;; (define (adjoin-points! l p)
;;   (set! l (adjoin-points p l)))

;; Shape
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

;; (define (shape-transformations s)
;;   (map (lambda (op) (op s))
;;        (list shape-refl-ud
;;              shape-refl-lr
;;              shape-transpose
;;              shape-rotate-180
;;              shape-rotate+90
;;              shape-rotate-90
;;              shape-refl-ulrd)))

;; like above but a little bit more efficient
(define (shape-transformations s)
  (let ((s-transposed (shape-transpose s))
        (s-ud-flipped (shape-refl-ud s)))
    (list s-transposed
          s-ud-flipped
          (shape-refl-lr s)
          (shape-rotate-180 s)
          (shape-refl-ud s-transposed) ;; rotate+90
          (shape-transpose s-ud-flipped) ;; rotate-90
          (shape-rotate-180 s-transposed))))

;; transformations including itself
(define (shape-transformations* s)
  (cons s (shape-transformations s)))

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
