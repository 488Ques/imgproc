; Thanks to Rosetta Code for the sample code
; https://rosettacode.org/wiki/Image_convolution#Racket

#lang typed/racket
(require images/flomap typed/racket/flonum)

(provide typed-flomap-convolve)

(: perfect-square? (Nonnegative-Fixnum -> (U Nonnegative-Fixnum #f)))
(define (perfect-square? n)
  (define rt-n (integer-sqrt n))
  (and (= n (sqr rt-n)) rt-n))

(: typed-flomap-convolve (flomap FlVector -> flomap))
(define (typed-flomap-convolve F V)
  (define R (perfect-square? (flvector-length V)))
  (cond
    [(not (and R (odd? R))) (error "K is not odd-sided square")]
    [else
     (define R/2 (quotient R 2))
     (define-values (sz-w sz-h) (flomap-size F))     
     (define-syntax-rule (convolution c x y i)
       (if (= 0 c)
           (flomap-ref F c x y)
           ; Some minor differences here from Rosetta Code's
           ; Because I found the produced image didn't meet expectation
           (for*/fold: : Flonum
               ((acc : Flonum 0.))
               ; Kernel is flipped horizontally and vertically before convolution
               ([kx (in-range (sub1 R) -1 -1)]     ; Kernel's horizontal index
                [ky (in-range (sub1 R) -1 -1)]     ; Kernel's vertical index
                [ki (in-value (+ (* ky R) kx))]    ; Map kernel's matrix index to vector index
                [xx (in-value (+ (- x kx) R/2))]   ; Input's horizontal index
                [yy (in-value (+ (- y ky) R/2))]   ; Input's vertical index
                #:when (and (< 0 xx (sub1 sz-w)) (< 0 yy (sub1 sz-h)))) ; Skip if input's index is out of bound
             (+ acc (* (flvector-ref V ki)
                       (flomap-ref F c xx yy))))))
     
     (inline-build-flomap 4 sz-w sz-h convolution)]))
