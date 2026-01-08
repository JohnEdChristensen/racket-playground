#lang racket
(require "run.rkt")
(require racket/draw)
(require math)
; one color, one shape
(define bg (make-color 0 0 0 1))
(define fg (make-color 250 0 200 1))

(define (draw-shape dc dx1 dy1 dx2 dy2)
  (send dc draw-polygon
        (list (cons 0 0)
              (cons dx1 dy1)
              (cons (+ dx1 dx2) (+ dy1 dy2))
              (cons dx2 dy2))))


(define (my-draw dc w h now)
  (let* ([transform (send dc get-transformation)])

    (send dc set-brush bg 'solid)
    (send dc draw-rectangle 0 0 w h)
    (send dc translate (/ w 2) (/ h 2))
    (send dc set-pen bg 2 'solid)
    (send dc set-brush  fg 'solid)
    (for ([i (in-range 500)])
      (begin
        (send dc rotate (* (* 2 pi)  (- 1 (/ 1 1.618))));(* (sin now) 0.2))
        (send dc translate (/ i (+ 0.0001 (sin now))) 0)
        (draw-shape dc 20 0 10 20)))
    (send dc set-transformation transform)))

;(run-app (lambda (dc w h) (my-draw dc w h)) bg)
(run-app (lambda (dc w h now) (my-draw dc w h now)) bg "26-01-01" 0)

