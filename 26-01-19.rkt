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
(define fg (spill 2 2 2))
(define fg1 (spill 2 8 4))
(define fg2 (spill 1 7 3))
(define line-width 4)

(define (coulomb q r01)
  (vs (/ q (expt (mag r01) 3))
      r01))

(define p1 (vec 0. 100.))
(define p2 (vec 0. -100.))

(define (draw-centered-segment dc p a l)
  (let* ([dp1 (vs (/ l 2.0) (vec (cos a) (sin a)))]
         [dp2 (vrot dp1 pi)]
         [s (segment (v+ p dp1) (v+ p dp2))])
  (draw-segment dc s fg 2)))
    


(define (my-draw dc w h now mp)
  (define mouse (v* (vec 1. -1.)
                    (v- mp (vec (/ w 2.) (/ h 2.)))))
  ;(displayln mouse)
  (set! p1 mouse)

  (define (my-e-field p)
    ;(displayln p1)
    (v+
     (coulomb 1 (v- p p1))
        (coulomb -1 (v- p p2))))

  (define (draw-lattice dc n d)
  (for*/list ([i (range 0. n)]
              [j (range 0. n)])
    (let* ([hw (* 0.5 (- n 1) d)]
           [x (- (* i d) hw)]
           [y (- (* j d) hw)]
           [f (my-e-field (vec x y))]
           [fmag (mag f)]
           [c fg1])
      (draw-centered-segment dc (vec x y) (atan (vec-x f) (vec-y f)) (* 0.9 d))
      ;(fill-circle dc (vec x y) (* 1000. fmag) fg1)
      )))
           ;(fill-circle dc (vec x y) (* 2000. (mag f)) c))))
  (send dc set-brush bg 'solid)
  ;(send dc set-pen off 1 'transparent)
  (send dc draw-rectangle 0 0 w h)

  ;(send dc set-brush off 'transparent)
  (send dc set-pen fg1 line-width 'solid)

  (send dc translate (/ w 2) (/ h 2))
  (send dc scale 1 -1)

  (draw-lattice dc 16 20)
  )


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
