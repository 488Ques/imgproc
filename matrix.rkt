#lang racket
(define (list->matrix lst col)
  (define (iter remains matrix)
    (define-values (row rems) (vector-split-at remains col))
    (if (vector-empty? rems)
        (vector-append matrix (vector row))
        (iter rems
              (vector-append matrix (vector row)))))
  (iter (list->vector lst) #()))

(define (matrix-row matrix)
  (vector-length matrix))

(define (matrix-col matrix)
  (vector-length (vector-ref matrix 0)))

(define (matrix->list matrix)
  (define ROW (matrix-row matrix))
  (define (iter lst count)
    (if (= count ROW)
        (vector->list lst)
        (iter (vector-append lst
                             (vector-ref matrix count))
              (+ count 1))))
  (iter #() 0))

(define (matrix-ref matrix row col)
  (vector-ref (vector-ref matrix row) col))

(provide list->matrix matrix-row matrix-col matrix->list matrix-ref)