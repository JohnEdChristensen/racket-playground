#lang racket
(require math/flonum)
(require racket/draw/unsafe/cairo
         (only-in ffi/unsafe ptr-ref ptr-set! _uint32)
         racket/require
         (filtered-in
          (lambda (name)
            (and (regexp-match #rx"^unsafe-fx" name)
                 (regexp-replace #rx"unsafe-" name "")))
          racket/unsafe/ops))


(define (render-bm bm dc scale render now)
  (let ([w (send bm get-width)]
        [h (send bm get-height)]
        [bm-dc (send bm make-dc)]
        [fast-setpixel (make-fast-setpixel bm)])
    ;render data
    (for* ([x (range w)]
           [y (range h)])
      (fast-setpixel x y (render  x  y (fl w) (fl h) now))
      )
    ;(print (fast-getpixel 32 32))
    ;draw to screen
    (send dc set-smoothing 'unsmoothed)
    (send dc scale scale scale)
    (send dc draw-bitmap bm 0 0)
    ;restore
    (send dc set-smoothing 'smoothed)))

(define (make-fast-getpixel bm)
  (define width (send bm get-width))
  (define height (send bm get-height))
  (define s (send bm get-handle))
  (define data (cairo_image_surface_get_data* s))
  (define stride (cairo_image_surface_get_stride s))
  (lambda (x y)
    (if (and (fx>= x 0) (fx< x width)
             (fx>= y 0) (fx< y height))
        (ptr-ref data _uint32 'abs
                 (fx+ (fx* y stride) (fxlshift x 2)))
        #f)))

(define (make-fast-setpixel bm)
  (define width (send bm get-width))
  (define height (send bm get-height))
  (define s (send bm get-handle))
  (define data (cairo_image_surface_get_data* s))
  (define stride (cairo_image_surface_get_stride s))
  (lambda (x y value)
    (if (and (fx>= x 0) (fx< x width)
             (fx>= y 0) (fx< y height))
        (ptr-set! data _uint32 'abs
                 (fx+ (fx* y stride) (fxlshift x 2)) value)
        #f)))


(provide make-fast-getpixel
         make-fast-setpixel
         render-bm)
