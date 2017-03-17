#lang slideshow

(require "shapes/polyo.rkt"
         "draw/draw-shape.rkt")

(define tetrominoes (gen-polyo-of 4 'one-sided))
(map shape->pict tetrominoes)
