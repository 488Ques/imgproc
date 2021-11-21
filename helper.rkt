#lang racket

(define (list-minmax lst)
  (define (iter remains I X)
    (if (empty? remains)
        (values I X)
        (iter (rest remains)
              (min I (first remains))
              (max X (first remains)))))
  (iter lst +inf.0 -inf.0))

(provide list-minmax)