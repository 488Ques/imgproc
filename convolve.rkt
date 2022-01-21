#lang racket
(require images/flomap
         racket/flonum)

(provide flomap-convolve)

(define (perfect-square? n)
  (let ([n-sqrt (sqrt n)])
    (and (= n (sqr n-sqrt)) n-sqrt)))

(define (flomap-convolve F V)
  (define R (perfect-square? (flvector-length V) ))
  (define R/2 (quotient R 2))
  (define-values (W H) (flomap-size F))
  (define comps (flomap-components F))
  (define (convolution c x y)
    (if (and (= 4 comps)  (= 0 c))
        (flomap-ref F c x y)
        (for*/fold ([acc 0])
                   ([kx (in-inclusive-range (sub1 R) 0 -1)]  ; Kernel column index
                    [ky (in-inclusive-range (sub1 R) 0 -1)]  ; Kernel row index
                    [xx (in-value (+ (- x kx) R/2))]         ; Input horizontal index
                    [yy (in-value (+ (- y ky) R/2))]         ; Input vertical index
                    #:when (and (< 0 xx (sub1 W)) (< 0 yy (sub1 H))))
          (+ acc (* (flvector-ref V (+ (* ky R) kx)) (flomap-ref F c xx yy))))))
  (build-flomap comps W H convolution))
