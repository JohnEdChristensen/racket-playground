#lang racket

(require "run.rkt"
         "draw.rkt"
         "geom.rkt"
         "shapes.rkt"
         racket/draw
         math/flonum
         racket/draw/arrow
         )

(define bg (spill 9 8 6))
(define off (make-color 0 0 0 0))
(define mg (make-color 150 150 150 1))
(define ch 7)
(define cl 3)
(define fg1 bg)
(define fg2 (spill cl ch ch))
(define fg3 (spill cl ch cl))
(define fg4 (spill ch cl cl))
(define line-width 15)
(define circle-r 24)
(define mouse (vec 0. 0.))

(define a 100.)
(define sqr2 (sqrt 2))

(define (lattice-index)
  (let* ([n 8.]
         [ns (range (- n)  n)])
    (cartesian-product ns ns)))

(define (lattice-pos v1 v2)
  (map (lambda (i)
         (v+ (vs (first i) v1)
            (vs (second i) v2)))
       (lattice-index)))
            

(define (draw-lattice dc v1 v2 draw)
  (for-each (lambda (o)
              (let ([p1 (v+ o v1)]
                    [p2 (v+ o v2)])
                (draw dc o p1 p2)))
            (lattice-pos v1 v2)))

(define (draw-wallpaper dc v1 v2)
  (draw-lattice dc v1 v2 draw-back)
  (draw-lattice dc v1 v2 draw-cell)
  (draw-lattice dc v1 v2 draw-inner))

(define (draw-back dc o p1 p2)
  (let*  ([mid (vbetween p1 p2)]
          [q1 (vbetween o mid)]
          [q3 (vbetween mid (v+ o (vs 2. (v- mid o))))])
    (fill-circle dc q1 (* 0.30 a) fg3)
    (fill-circle dc q3 (* 0.30 a) fg4)
    ))

(define (draw-cell dc o p1 p2)
  (draw-segment dc (segment o p1) fg1 line-width)
  (draw-segment dc (segment o p2) fg1 line-width))

(define (draw-inner dc o p1 p2)
  (let*  ([mid (vbetween p1 p2)]
          [r (- (* 0.7 0.5 (magnitude (v- p1 p2)))
                (* 0.5 line-width))]
          [mid-to-mouse (norm (v- mouse mid))])
  (fill-circle dc mid r fg2)
  (fill-circle dc mid (/ r 1.75) fg1)
  (fill-circle dc (v+ mid (vs 8. mid-to-mouse)) (/ r 3) fg2)))

(define (my-draw dc w h now mp)
  (define a1 (/ pi 4))
  (define a2 (/ pi -4))
  (define v1 (vs a (vec (cos a1) (sin a1))))
  (define v2 (vs a (vec (cos a2) (sin a2))))
  (send dc set-brush bg 'solid)
  (send dc set-pen off 1 'transparent)
  (send dc draw-rectangle 0 0 w h)

  (send dc set-brush off 'transparent)
  (send dc set-pen fg1 line-width 'solid)

  (send dc translate (/ w 2) (/ h 2))
  (send dc scale 1 -1)

  ;(set! mouse (v*
  ;               (v- mp
  ;                   (vec (/ w 2.) (/ h 2.)))
  ;               (vec 1. -1.)))
  (define t (* now 2 pi 0.1))
  (set! mouse (vs (fl (/ w 2)) (vec (* 2. (cos (* 0.5 t))) (sin t))))

  (draw-wallpaper dc v1 v2))


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
