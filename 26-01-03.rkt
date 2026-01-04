#lang racket
(require "run.rkt")
(require pict)
(require racket/draw)

(define bg (make-color 0 0 0 1))
(define fg (make-color 255 255 255  1))

(define (shape size)
  (colorize (disk size #:border-color bg) fg ))

(define (combine a b angle)
  (let* ([next-width (+ (pict-width a) (pict-width b))]
         [inner (hc-append a b)]
         [outer (linewidth (* 0.08 next-width) (shape next-width))]
         [correction (- (abs (* next-width  0.5 (- (sqrt 2) 1) (sin (* angle 2)))))])
    (inset (rotate
            (cc-superimpose outer inner)
            angle) correction)))
                
(define (fib n a)
  (cond ([= n 0] (blank))
        ([= n 1] (shape 10))
        (else (combine (fib (- n 1) a)
                 (fib (- n 2) a) a))))

(define (my-draw dc w h now)
  (let* ([transform (send dc get-transformation)])
    (send dc set-smoothing 'smoothed)
    (send dc set-brush bg 'solid)
    (send dc draw-rectangle 0 0 w h)

    (define p (fib 11 (+ (/ pi 2) (* 2 pi  0.025 now))))
    (draw-pict  (scale-to-fit p w h #:mode 'inset)  dc 0 0 )

    (send dc set-transformation transform)))

(run-app (lambda (dc w h now) (my-draw dc w h now))
         (make-color 0 0 0 1)
         "26-01-03"
         400
         0.1)
