#lang racket

(define (list-min l)
  (define (iter m l)
    (if (empty? l)
        m
        (iter (min m (first l)) (rest l))))
  (iter +inf.0 l))

(define (list-max l)
  (define (iter m l)
    (if (empty? l)
        m
        (iter (max m (first l)) (rest l))))
  (iter -inf.0 l))

(provide list-min list-max)