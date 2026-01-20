#lang racket

(require "run.rkt"
         "draw.rkt"
         "geom.rkt"
         racket/draw
         )

(define bg (make-color 0 0 0 1))
(define off (make-color 0 0 0 0))
(define fg (make-color 255 255 255 1))

(define t-scale 0.03)
(define x-scale 100)
(define y-scale 75)
(define n 16)
(define periods 3)

(define (xs start end n)
  (range start end (/ (- end start) n)))

(define (draw-series-square dc f t xs)
    (for-each (lambda  (x)
                (draw-square dc (* x x-scale) (* y-scale (f (+ x (* t t-scale))) )))
              xs))

(define dot-r 2.5)
(define (draw-series-dot dc f t xs)
  (for-each (lambda  (x)
              (send dc draw-ellipse (- (* x x-scale) dot-r)
                    (- (* y-scale (f (+ x (* t t-scale)))) dot-r)
                    (* 2 dot-r) (* 2 dot-r)))
              xs))

(define (n-sin x)
  (sin (* 2 pi x)))

(define (frac-part n)
  (- n (truncate n)))
  
(define (n-saw x)
  (- (* 2 (frac-part x)) 1))
  
(define (n-tri x)
  (- (* 4  (abs (- (frac-part x) 0.5))) 1))

(define (make-linear m)
  (lambda (x)
    (* x m)))

(define (make-piecewise f1 f2 threshold)
  (lambda (x)
    (if (< x threshold)
        (f1 x)
        (f2 x))))

(define (draw-square dc x h)
  ;(display h)
  (if (> h 0 )
      (send dc draw-rectangle x 0 h h)
      (send dc draw-rectangle x  h  (- h)  (- h))))

(define xs1 (xs 0 periods (* n periods) ))
(define (my-draw dc w h now)
  (send dc set-brush bg 'solid)
  (send dc set-pen fg 1 'transparent)
  (send dc draw-rectangle 0 0 w h)

  (send dc set-brush off 'transparent)
  (send dc set-pen fg 1 'solid)
  (send dc translate 50 50)

  (send dc scale 1 -1)

  (send dc translate 0 (- y-scale))
  ;(draw-series-dot dc n-sin now xs1)
  (draw-series-square dc n-sin now xs1)

  (send dc translate 0 (- (* 2.5 y-scale)))
  (draw-series-square dc n-saw now xs1)
  ;(draw-series-dot dc n-saw now xs1)

  (send dc translate 0 (- (* 2.5 y-scale)))
  (draw-series-square dc n-tri now xs1)
  ;(draw-series-dot dc n-tri now xs1)

  ;(send dc translate 0 (- (* 2.5 scale)))
  ;(draw-series dc n-saw 3 16)
)

(run-app (lambda (dc w h now)  (my-draw dc w h now))
         (make-color 0 0 0 1)
         "26-01-12"
         0
         1.0
         450
         650
         )
