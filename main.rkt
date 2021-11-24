#lang racket
(require 2htdp/image
         "matrix.rkt"
         "processing.rkt"
         "kernels.rkt")

(define target (bitmap/file "img.jpg"))

(define (transform image kernel)
  (define COL (image-width image))
  (define ROW (image-height image))
  (define pixels (image->color-list image))
  
  (define (extract-channels clrlst)
    (define (extract-channel channel)
      (list->matrix (map (lambda (clr) (channel clr)) clrlst) COL))
    (values (extract-channel color-red)
            (extract-channel color-green)
            (extract-channel color-blue)))
  
  (define (process-channels R G B kernel)
    (define (process-channel channel)
      (processing channel kernel))
    (values (process-channel R)
            (process-channel G)
            (process-channel B)))

  (define (join-channels R G B)
    (for/list ([r R]
               [g G]
               [b B])
      (make-color r g b)))
  
  (define-values (R G B) (extract-channels pixels))
  (define-values (proc-R proc-G proc-B) (process-channels R G B kernel))
  (define proc-clrlst (join-channels proc-R
                                     proc-G
                                     proc-B))
  (color-list->bitmap proc-clrlst COL ROW))