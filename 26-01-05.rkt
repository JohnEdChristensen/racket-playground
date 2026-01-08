#lang racket
(require "run.rkt")
(require "draw.rkt")
(require "geom.rkt")
(require "sdf.rkt")
(require racket/draw)

(define bg (make-color 0 0 0 1))
(define fg (make-color 255 0 255  1))

(define scale 2)
(define res-x 256)
(define res-y 128)
(define bm (make-bitmap res-x res-y #f))
(define fast-setpixel (make-fast-setpixel bm))


;(: render (-> Float Float Float Float Float Integer))
(define font-rad 0.1)

(define (render i j w h now)
  (let* (
        [x (/ (- (* 2 i) h) w)]
        [y (/ (- (* 2 j) h) w)]
        [p (vec (+ x 0.25) (+ y 0.05))])

    (sdf-color  (- (min (sdf-g p)
                     (sdf-e (v- p (vec (* 2.5 font-rad) 0.0)))
                     (sdf-n (v- p (vec (* 5.0 font-rad) 0.0)))
                     (sdf-u (v- p (vec (* 7.5 font-rad) 0.0)))
                     (sdf-a (v- p (vec (* 10 font-rad) 0.0)))
                     (sdf-r (v- p (vec (* 12.5 font-rad) 0.0)))
                     (sdf-y (v- p (vec (* 15 font-rad) 0.0)))
                     )
                   (* 0.010 (sin (* 2 pi  0.2 now )) (+ (sin (* 20 x))
                       (sin (* 20 y)) ))
                   ))))


(define (my-draw dc w h now)
    (send dc set-smoothing 'smoothed)
    (send dc set-brush bg 'solid)
  (send dc draw-rectangle 0 0 w h)
  (define start (current-inexact-milliseconds))


  (render-bm bm dc scale render now)
  (println (- (current-inexact-milliseconds) start))
  )

(run-app (lambda (dc w h now) (my-draw dc w h now))
         (make-color 0 0 0 1)
         "26-01-05"
         0 
         0.05
         (* res-x scale )
         (* res-y scale )
         )
