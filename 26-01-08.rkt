#lang racket

(require "run.rkt"
         "geom.rkt"
         racket/draw)

(define bg (make-color 0 0 0 1))
(define fg (make-color 255 255 255 1))


(define (my-draw dc w h now)
  (println "draw")
  (send dc set-pen fg  2.0 'solid)
  (send dc set-brush fg 'solid)
  (send dc draw-rectangle 0 0 10 10)
  )

(run-app (lambda (dc w h now)  (my-draw dc w h now))
         (make-color 0 0 0 1)
         "26-01-08"
         0
         0.1)
