#lang racket/base

;; this module provides general transformations on coordinates,
;; including:
;;     reflections
;;     rotations
;;     translations

(provide (all-defined-out))

;; the 'coord' struct
(struct coord (x y) #:prefab)

;; reflections
(define (coord-refl-ud p y-ref)
  (coord (coord-x p) (- (* 2 y-ref) (coord-y p))))

(define (coord-refl-lr p x-ref)
  (coord (- (* 2 x-ref) (coord-x p)) (coord-y p)))

(define (coord-refl-origin p origin)
  (let ((origin-x (coord-x origin))
        (origin-y (coord-y origin)))
    (coord (- (* 2 origin-x) (coord-x p))
           (- (* 2 origin-y) (coord-y p)))))

(define (coord-transpose p)
  (coord (coord-y p) (coord-x p)))

(define (coord-rotate+90 p origin)
  (coord-refl-ud (coord-transpose p) (coord-x origin)))

(define (coord-rotate-90 p origin)
  (coord-transpose (coord-refl-ud p origin)))

(define (coord-refl-ulrd p origin)
  (coord-refl-origin (coord-transpose p) (coord-y origin)))

;; translation
(define (coord-shift-x p diff)
  (coord (+ (coord-x p) diff) (coord-y p)))

(define (coord-shift-y p diff)
  (coord ((coord-x p) (+ (coord-y p) diff))))

(define (coord-shift p vec)
  (coord
   (+ (coord-x p) (coord-x vec))
   (+ (coord-y p) (coord-y vec))))
