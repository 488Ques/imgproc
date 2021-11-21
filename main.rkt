#lang racket
(require 2htdp/image
         "matrix.rkt"
         "processing.rkt"
         "kernels.rkt")

;(define target (bitmap/file "img.jpg"))

(define (color-list->grayscale colorlist)
  (for/list ((color colorlist))
    (define r (color-red color))
    (define g (color-green color))
    (define b (color-blue color))
    (define gs (exact-floor (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))
    gs))

(define (grayscale->color-list grayscale)
  (for/list ([gs grayscale])
    (make-color gs gs gs)))

(define (transform image kernel)
  (define WIDTH (image-width image))
  (define HEIGHT (image-height image))

  (define cl (image->color-list image))
  (define gs (color-list->grayscale cl))
  (define gs-matrix (list->matrix gs WIDTH))
  (define processed (processing gs-matrix kernel))
  (define processed-cl (grayscale->color-list processed))
  
  (color-list->bitmap processed-cl WIDTH HEIGHT))