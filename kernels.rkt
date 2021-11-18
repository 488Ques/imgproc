#lang racket

(define identity (list 0 0 0
                       0 1 0
                       0 0 0))

(define sharpen (list 0 -1 0
                      -1 5 -1
                      0 -1 0))

(define gaus-blur (list 0.0625 0.125 0.0625
                        0.125 0.25 0.125
                        0.0625 0.125 0.0625))

(define box-blur (list 1/9 1/9 1/9
                       1/9 1/9 1/9
                       1/9 1/9 1/9))

(define edge (list -1 -1 -1
                   -1 8 -1
                   -1 -1 -1))

(provide (all-defined-out))