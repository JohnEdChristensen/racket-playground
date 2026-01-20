#lang racket

(require "run.rkt"
         "draw.rkt"
         "geom.rkt"
         "shapes.rkt"
         racket/draw
         racket/gui
         math/flonum
         racket/draw/arrow
         )

(define bg (spill 9 8 6))
(define fg (spill 2 8 4))
(define line-width 4)

(define (my-draw dc w h now mp)
  (send dc set-brush bg 'solid)
  ;(send dc set-pen off 1 'transparent)
  (send dc draw-rectangle 0 0 w h)

  ;(send dc set-brush off 'transparent)
  (send dc set-pen fg line-width 'solid)

  (send dc translate (/ w 2) (/ h 2))
  (send dc scale 1 -1)

  (fill-circle dc (vec 0. 0.) 50 fg))


(define dur-s 20.)
(define dt 0.1)
(define n (truncate (/ dur-s dt)))

(define fc (run-app (lambda (dc w h now mp)  (my-draw dc w h now mp))
         (make-color 0 0 0 1)
         "26-01-18"
         0
         dt
         400
         400
         ))
(define canvas (second fc))

(send canvas on-event (lambda (e)
                        (display e)))

(define f (new frame% [label "hi"]))

(define (spill-slider l) (new slider%
                    (label l)
                    (parent f)
                    (min-value 0)
                    (max-value 9)
                    (init-value 5)
                    (style (list 'horizontal 'plain))
                    (callback (lambda (v a) (displayln (send v get-value))))
                    ))
(spill-slider "r")
(spill-slider "g")
(spill-slider "b")
(send f show #t)
