#lang racket

(require (only-in racket/draw read-bitmap)
         (only-in images/flomap bitmap->flomap flomap->bitmap)
         (only-in racket/flonum flvector)
         (only-in 2htdp/image save-image)
         "typed-convolve.rkt")

;; (define flmp (bitmap->flomap (read-bitmap "img.jpg")))

(define (render-convoluted flmp kernel)
  (flomap->bitmap (typed-flomap-convolve flmp
                                         kernel)))

;; (define t (transform flmp (flvector 0. -1. 0. -1. 5. -1. 0. -1. 0.)))

;; (save-image t "t.png")
