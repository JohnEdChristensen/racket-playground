#lang racket
(require racket/draw
         racket/draw/arrow
         )
(require "geom.rkt")

(define (scale-spill v)
  (truncate (/ (* v 255) 9 )))

(define (spill r g b)
  (make-color (scale-spill r)
              (scale-spill g)
              (scale-spill b)
              1))


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

(define (draw-circle dc v r color)
  (send dc set-pen color 1 'solid)
  (send dc set-brush color 'transparent)
  (send dc draw-ellipse (- (vec-x v) r) (- (vec-y v) r) (* 2 r) (* 2 r)))

(define (fill-circle dc v r color)
  (send dc set-pen color 0 'solid)
  (send dc set-brush color 'solid)
  (send dc draw-ellipse (- (vec-x v) r) (- (vec-y v) r) (* 2 r) (* 2 r)))

(define (draw-segment dc s color line-width)
  (let* (
         [p1 (segment-start s)]
         [p2 (segment-stop s)]
         [x1 (vec-x p1)]
         [y1 (vec-y p1)]
         [x2 (vec-x p2)]
         [y2 (vec-y p2)]
         )
    (send dc set-pen color line-width 'solid)
    (send dc draw-line x1 y1 x2 y2)))

(define (draw-vec dc s color line-width)
  (let* (
         [p1 (segment-start s)]
         [p2 (segment-stop s)]
         [x1 (vec-x p1)]
         [y1 (vec-y p1)]
         [x2 (vec-x p2)]
         [y2 (vec-y p2)]
         )

  (send dc set-pen color line-width 'solid)
  (send dc set-brush color 'solid)
  (draw-arrow dc x1 y1 x2 y2 0 0)))
  ; #:arrow-head-size 0.1)))

(provide draw-grid
         draw-series
         draw-circle
         fill-circle
         draw-segment
         draw-vec
         spill)
