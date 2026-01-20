#lang racket

(require "run.rkt"
         "draw.rkt"
         "geom.rkt"
         racket/draw
         )

(define bg (make-color 0 0 0 1))
(define fg (make-color 255 255 255 1))

(define scale 4)
(define res 128)
(define bm (make-bitmap res res #f))

(define (sdf-sphere p r)
  (- (v3magnitude p) r))

(define (sdg-sphere p r)
  (v3s (/ 1 (v3magnitude p)) p))



(define (render i j w h now)
  (define (ray-cast p n)
    (let ([d (sdf-sphere p 0.5)])
      (cond ((or (<= n 0) (> (vec3-z p) 1.0))
             (rgb-u32 0 0 0))
            ((< d 0.01)
             (let ([light (max 0.02 (v3dot (v3norm (vec3 (cos now) -1 (sin now)))
                                           (sdg-sphere p 1.0)))])
               (rgb-u32
                (exact-round (* light 255))
                (exact-round (* light 255))
                (exact-round (* light 255)))))
            (else (ray-cast (v3+ p (vec3 0. 0. d)) (- n 1))))))
  (let* ([x (/ (- (* 2 i) w) h)]
        [y (/ (- (* 2 j) w) h)]
        [ray-dir (vec3 0. 0. 1.)]
        [ray-pos (vec3 x y -2)])
    (ray-cast ray-pos 6)))



(define (my-draw dc w h now)
  ;(send dc set-pen fg  2.0 'solid)
  (send dc set-brush bg 'solid)
  (send dc draw-rectangle 0 0 w h)

  (render-bm bm dc scale render now)
  )

(run-app (lambda (dc w h now)  (my-draw dc w h now))
         (make-color 0 0 0 1)
         "26-01-08"
         0
         1.0)
