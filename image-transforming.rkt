#lang racket
(require 2htdp/image
         "processing.rkt"
         "kernels.rkt")

; (define target (bitmap/file "img.jpg"))

(define (color-list->grayscale-vector colorlist)
  (for/vector ((color colorlist))
    (define r (color-red color))
    (define g (color-green color))
    (define b (color-blue color))
    (define gs (exact-floor (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))
    gs))

(define (grayscale->color-list grayscale)
  (for/list ([gs grayscale])
    (make-color gs gs gs)))

(define (grayscale->image m)
  (define mat (matrix-mat m))
  (define width (matrix-col m))
  (define height (matrix-row m))
  (color-list->bitmap (grayscale->color-list mat) width height))

(define (transform-img img kernel)
  (let ([gs-matrix (make-matrix (color-list->grayscale-vector (image->color-list img))
                                (image-height img)
                                (image-width img))])
    (grayscale->image (processing gs-matrix
                                  kernel))))