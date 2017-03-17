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
