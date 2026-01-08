#lang racket
(require "run.rkt")
(require "draw.rkt")
(require "geom.rkt")
(require racket/draw)

(define bg (make-color 0 0 0 1))
(define fg (make-color 255 0 255  1))

(define scale 4)
(define res 128)
(define bm (make-bitmap res res #f))
(define fast-setpixel (make-fast-setpixel bm))

(define (sdf-color v)
  (let ([phase (+ 0.4 (* 0.3 ( + 1 (cos (* 50 v)))))])
    (cond ((negative? v) (rgb-u32
                          (exact-round (* phase 180))
                          (exact-round (* phase 100))
                          (exact-round (* phase 240))))
          (else (rgb-u32 (exact-round (* phase 100))
                          (exact-round (* phase 200))
                          (exact-round (* phase 220)))))))


(define (circle-sdf p r)
  (- (magnitude p) r))

(define (render i j w h now)
  (let* (
        [x (/ (- (* 2 i) w) h)]
        [y (/ (- (* 2 j) w) h)]
        [p (vec x y)])

    (sdf-color (min (circle-sdf (v- p (vec 0.2 -0.2)) 0.3)
                  (circle-sdf (v- p (vec -0.2 0.5)) 0.2)))))

(define (render-simple i j w h)
 ; (let* (
 ;       [x (/ (- (* 2 i) w) h)]
 ;       [y (/ (- (* 2 j) w) h)]

    (rgb-u32 (modulo i 255) (modulo j 255) 1 ))

(define (my-draw dc w h now)
    (send dc set-smoothing 'smoothed)
    (send dc set-brush bg 'solid)
    (send dc draw-rectangle 0 0 w h)

    (render-bm bm dc scale render now))

(run-app (lambda (dc w h now) (my-draw dc w h now))
         (make-color 0 0 0 1)
         "26-01-04"
         0
         0.1)
