#lang slideshow

(require "shapes/polyo.rkt"
         "shapes/shape.rkt"
         "draw/draw-shape.rkt"
         "commons/helper.rkt")

(define seq3 (gen-fixed-polyo-of 3))
(define c5 (gen-fixed-polyo-upto 5))
(define seq5free (gen-free-polyo-of 5))
(define d5 (gen-free-polyo-upto 5))

(define seq6 (gen-free-polyo-of 6))
(define s12 (list-ref seq6 12))
(define s15 (list-ref seq6 15))

(map shape->pict seq3)
