#lang slideshow

(require "../shapes/shape.rkt"
         "../commons/helper.rkt")

(provide tile blank-tile shape->pict)

(define tile-size 40)
(define tile-border 2)
(define tile-color "Chocolate")
(define border-color "Ivory")

(define (make-tile #:size [a tile-size]
                   #:border-width [w tile-border]
                   #:color [color tile-color]
                   #:border-color [color-b border-color]
                   #:radius [r -0.15])
  (filled-rounded-rectangle a a r
                            #:color color
                            #:border-color color-b #:border-width w))

(define tile (make-tile))
(define blank-tile (make-tile #:color border-color))

(define (matrix->pict mat pic blank-pic)
  (apply vc-append
         (map (lambda (row)
                (apply hc-append row))
              (deepmap (lambda (x)
                         (if (= 1 x) pic blank-pic))
               mat))))

(define (shape->pict s)
  (matrix->pict (shape->matrix s) tile blank-tile))
