#lang racket
(require "helper.rkt"
         "matrix.rkt")

(define (processing matrix kernel)
  (define ROW (matrix-row matrix))
  (define COL (matrix-col matrix))

  ; Find neighbor pixels given a pixel's position
  (define (neighbors row col)
    ; Edge handling by wrapping
    (let ([prev (remainder (+ ROW (- row 1)) ROW)]
          [current row]
          [next (remainder (+ row 1) ROW)])
      (define (ref r c) (matrix-ref matrix r c))
      (define (pref c) (ref prev c))
      (define (cref c) (ref current c))
      (define (nref c) (ref next c))
      (define col-list (list (remainder (+ COL (- col 1)) COL)
                             col
                             (remainder (+ col 1) COL)))
    
      (append (map pref col-list)
              (map cref col-list)
              (map nref col-list))))

  (define (convolution 3x3)
    (foldl (lambda (ele1 ele2 result)
             (+ result (* ele1 ele2)))
           0
           3x3 kernel))

  (define (normalize pixels)
    (define min (list-min pixels))
    (define max (list-max pixels))
    (define Nmin 0)
    (define Nmax 255)
    (map (lambda (pixel)
           (floor (* (- pixel min) (/ (- Nmax Nmin) (- max min)))))
         pixels))

  (define 3x3s
    (for*/list ([row ROW]
                [col COL])
      (neighbors row col)))
  
  (define convolved-pixels
    (for/list ([3x3 (in-list 3x3s)])
      (convolution 3x3)))
  
  (normalize convolved-pixels))

(provide processing)