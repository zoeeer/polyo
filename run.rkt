#lang slideshow

(require "shapes/polyo.rkt"
         "shapes/shape.rkt"
         "draw/draw-shape.rkt"
         "commons/helper.rkt")

;; gen-fixed-polyo
(define seq3 (gen-fixed-polyo-of 3))
(define c5 (gen-fixed-polyo-upto 5))
(define seq5free (gen-free-polyo-of 5))
(define d5 (gen-free-polyo-upto 5))

;; gen-free-polyo
(define seq6 (gen-free-polyo-of 6))
(define s12 (list-ref seq6 12))
(define s15 (list-ref seq6 15))

;; gen-polyo given type
(define f5 (gen-polyo-of 5 'fixed))
(define fr5 (gen-polyo-of 5 'free))
(define fr6 (gen-polyo-of 6 'free))

(map shape->pict fr5)

;;
(define (test-range func)
  (display func)
  (map (lambda (n)
         (length (func n)))
       (range 9)))

(test-range gen-fixed-polyo-of)
(test-range gen-free-polyo-of)

(define (test-gen-polyo type)
  (display type)
  (map (lambda (n)
         (length (gen-polyo-of n type)))
       (range 9)))

(test-gen-polyo 'free)
(test-gen-polyo 'fixed)
(test-gen-polyo 'one-sided)

(define tetrominoes (gen-polyo-of 4 'one-sided))

(map shape->pict tetrominoes)
