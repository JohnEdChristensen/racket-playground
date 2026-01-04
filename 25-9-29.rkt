#lang racket
(require "run.rkt")
(require racket/draw)
(require math)

(define bg (make-color 50 50 80 1))
(define bg2 (make-color 200 200 200 1))
(define fg (make-color 200 50 50 1))
(define fg2 (make-color 50 200 50 1))

(define (draw-grid dc w h)
  (let ([center-x (/ w 2)]
        [center-y (/ h 2)])
    (send dc set-pen "gray" 1 'solid)
    (send dc translate center-x center-y)
    (send dc draw-line 0 h 0 (* -1 h))
    (send dc draw-line w 0 (* -1 w) 0)))

(define (draw-series dc xs ys color)
  (send dc set-pen color 3 'solid)
  (send dc draw-lines (map cons xs ys)))

(define (wave a f p w t)
  (lambda (x) (* a (sin (+ (* x f) (* t w) p)))))

(define (draw-circle dc x y r color)
  (send dc set-pen color 1 'solid)
  (send dc set-brush color 'transparent)
  (send dc draw-ellipse (- x r) (- y r) (* 2 r) (* 2 r)))

(define (my-draw dc w h)
  (let* ([transform (send dc get-transformation)]
         [now (* 0.001 (current-milliseconds))]
         [xs (inclusive-range (* -0.5 w) (* 0.5 w) (/ w 200))]
         [a (* 0.4 h)]
         [f 0.01]
         [omega 0.4]
         [f1 (wave a f 0 omega now)]
         [f2 (wave a f (/ pi 2) omega now)])
    (draw-grid dc w h)

    (draw-circle dc 0 0 a bg2)

    (send dc draw-line 0 (f2 0) (f1 0) (f2 0))
    (send dc draw-line (f1 0) 0 (f1 0) (f2 0))

    (draw-series dc xs (map f2 xs) fg)
    (draw-series dc (map f1 xs) xs fg2)

    (draw-circle dc (f1 0) (f2 0) 5 bg2)
    (draw-circle dc (f1 0) 0 5 bg2)
    (draw-circle dc 0 (f2 0) 5 bg2)

    (send dc set-transformation transform)))

(run-app (lambda (dc w h) (my-draw dc w h)) (make-color 50 50 100 1))
