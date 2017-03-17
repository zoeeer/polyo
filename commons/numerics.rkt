#lang racket/base

;; numerics.rkt
;; tool box to work with numbers

(provide (all-defined-out))

(define make-range cons)
(define range-min car)
(define range-max cdr)

;; find both max and min for a list of numbers

;; (define (range-of lst)
;;   (define (iter lst min-val max-val)
;;     (cond ((null? lst)
;;            (make-range min-val max-val))
;;           (else
;;            (let ((x (car lst)))
;;              (cond ((x > max-val)
;;                     (iter (cdr lst) min-val x))
;;                    ((x < min-val)
;;                     (iter (cdr lst) x max-val))
;;                    (else
;;                     (iter (cdr lst) min-val max-val)))))))
;;   (if (null? lst)
;;       '()
;;       (iter (cdr lst) (car lst) (car lst))))

;; using accumulator (folding)
(define (range-of lst)
  (define op (lambda (x a-range)
               (cond ((< x (range-min a-range))
                      (make-range x (range-max a-range)))
                     ((> x (range-max a-range))
                      (make-range (range-min a-range) x))
                     (else a-range))))
  (if (null? lst)
      '()
      (foldl op (make-range (car lst) (car lst)) (cdr lst))))

;; span
(define (span lst)
  (let ((lst-range (range-of lst)))
    (if (null? lst-range)
        '()
        (- (range-max lst-range) (range-min lst-range)))))
        ;   1))) ;; +1 might need re-consideration
