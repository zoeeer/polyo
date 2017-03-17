#lang racket/base

(provide (all-defined-out))

;; flatmap
;; flaten "list of lists" into a list
(define (flatmap proc seq)
  (foldl append null (map proc seq)))

;; deepmap
;; map throught a nested list
(define (deepmap proc lst)
  (map (lambda (l)
         (map proc l))
       lst))
