#lang racket
(require "helper.rkt")

(struct matrix (mat row col))

; Mat is internal representation of matrix
(define (make-matrix mat row col)
  (matrix mat row col))

(define (matrix-ref matrix row col)
  (vector-ref (matrix-mat matrix) (+ col (* row (matrix-col matrix)))))

; Handle 3x3 kernel and grayscaled image only
(define (processing matrix kernel)
  ; Matrix is of matrix type
  ; Kernel is a list of 9 elements

  (define ROW (matrix-row matrix))
  (define COL (matrix-col matrix))
  
  (define (3x3 row col)
    ; Edge handling by wrapping
    (let ([prev (remainder (+ ROW (- row 1)) ROW)]
          [current row]
          [next (remainder (+ row 1) ROW)])
      (define (ref r c)
        (matrix-ref matrix r c))
      (define (pref c) (ref prev c))
      (define (cref c) (ref current c))
      (define (nref c) (ref next c))
      (define col-list (list (remainder (+ COL (- col 1)) COL) col (remainder (+ col 1) COL)))
    
      (append (map pref col-list)
              (map cref col-list)
              (map nref col-list))))

  (define 3x3s
    (for*/list ([row ROW]
                [col COL])
      (3x3 row col)))

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

  (make-matrix (normalize (for/list ([3x3 (in-list 3x3s)])
                            (convolution 3x3)))
               ROW COL))

(define img (make-matrix (vector 215 194 107 227
                                 197 140 82 119
                                 20 38 119 102 142)
                         3 4))

(provide processing make-matrix (struct-out matrix))