#lang racket
(require "run.rkt")
(require racket/draw)

(define bg (make-color 0 0 0 1))
(define fg (make-color 255 0 255  1))

(define bm (make-bitmap 128 128 #f))

(define (render-bm bm dc)
  (let ([bm-dc (send bm make-dc)])
    (send bm-dc set-pixel 0 0 fg)
    (send dc set-smoothing 'unsmoothed)
    (send dc scale 4 4)
    (send dc draw-bitmap bm 0 0)
    (send dc set-smoothing 'smoothed)))


(define (my-draw dc w h now)
  (let* ([transform (send dc get-transformation)])
    (send dc set-smoothing 'smoothed)
    (send dc set-brush bg 'solid)
    (send dc draw-rectangle 0 0 w h)

    (render-bm bm dc)


    (send dc set-transformation transform)))

(run-app (lambda (dc w h now) (my-draw dc w h now))
         (make-color 0 0 0 1)
         "26-01-04"
         0
         0.1)
