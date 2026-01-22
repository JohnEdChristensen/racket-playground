#lang racket

(require "run.rkt"
         "draw.rkt"
         "geom.rkt"
         "shapes.rkt"
         "turtle.rkt"
         "l-system.rkt"
         racket/draw
         math/flonum
         racket/draw/arrow)


(define k-variables '(F))
(define k-axiom '(F))
(define (k-rules X)
  (cond [(eq? X 'F) '(F + F - F - F + F)]
        [else (list X)]))

(define k-ls (step k-rules k-axiom 3))


(define h-variables '(A B))
(define h-axiom '(A))
(define (h-rules X)
  (cond [(eq? X 'A) '(+ B F - A F A - F B +)]
        [(eq? X 'B) '(- A F + B F B + F A -)]
        [else (list X)]))

(define ls (step h-rules h-axiom 5))

(define bg (spill 9 8 6))
(define fg (spill 2 4 2))
(define line-width 3)

(define (my-draw dc w h now mp)
  (send dc set-brush bg 'solid)
  (send dc set-pen bg 1 'transparent)
  (send dc draw-rectangle 0 0 w h)

  (send dc set-brush bg 'transparent)
  (send dc set-pen fg line-width 'solid)

  (send dc translate (/ w 16) (/ h 16))
  (send dc scale 1 -1)

  (draw-l-system dc ls))


(define dur-s 20.)
(define dt 0.1)
(define n (truncate (/ dur-s dt)))

(define fc (run-app (lambda (dc w h now mp)  (my-draw dc w h now mp))
         (make-color 0 0 0 1)
         "26-01-20"
         0
         dt
         400
         400
         ))
