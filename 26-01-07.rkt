#lang racket
(require "run.rkt")
(require "draw.rkt")
(require "geom.rkt")
(require racket/draw)
(require (rename-in racket/gui
                    (magnitude gui-magnitude)))

(require sgl sgl/gl-vectors)

(define bg (make-color 0 0 0 1))
(define fg (make-color 255 0 255  1))

(define scale 2)
(define res-x 128)
(define res-y 128)
(define bm (make-bitmap res-x res-y #f))
(define fast-setpixel (make-fast-setpixel bm))


(define font-rad 0.1)

(define (render i j w h now)
    (rgb-u32 (modulo i 255) (modulo i 255) (modulo j 255))
    )



(define (my-draw dc w h now)
  ;(send dc set-smoothing 'smoothed)
  ;(send dc set-brush bg 'solid)
  ;(send dc draw-rectangle 0 0 w h)
  (define start (current-inexact-milliseconds))
  ;(define gl-bm (make-gl-bitmap w h  (new gl-config%)))
  (define g (send dc get-gl-context))

  (send g call-as-current (thunk
                           (lambda ()
                           (gl-clear-color 255.0 255.0 255.0 1.0)
                           (gl-clear)
                           (gl-begin 'triangles)
                           (gl-vertex 1 2 3)
                           (gl-vertex-v (gl-float-vector 1 2 3 4))
                               (gl-end)

                               )))


  ;(render-bm bm dc scale render now)
  ;(println (- (current-inexact-milliseconds) start))
  )

(run-app (lambda (dc w h now) (my-draw dc w h now))
         (make-color 0 0 0 1)
         "26-01-05"
         0 
         0.05
         (* res-x scale )
         (* res-y scale ))
