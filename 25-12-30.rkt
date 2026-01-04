#lang racket

(require "run.rkt")
(require racket/draw)
(require math)

(define bg (make-color 50 50 80 1))
(define bg2 (make-color 200 200 200 1))
(define fg (make-color 200 50 50 1))
(define fg2 (make-color 50 200 50 1))

(define cell-size 10)
(define grid-pos-x 10)
(define grid-pos-y 10)
(define grid-size-x 8)
(define grid-size-y 20)

(define pieces '('s 'i 'z 'n 't 'j 'l))
(define (piece-geometry piece)
  (match piece
    ['s '((0 0)
          (0 1)
          (1 0)
          (1 1))]
    ['i '((0 0)
          (0 1)
          (0 2)
          (0 3))]
    ['z '((0 0)
          (1 0)
          (1 1)
          (2 1))]
    ['n '((1 0)
          (1 1)
          (0 1)
          (2 0))]
    ['t '((0 0)
          (1 0)
          (1 1)
          (2 0))]
    ['j '((0 0)
          (0 1)
          (0 2)
          (1 2))]
    ['l '((1 0)
          (1 1)
          (1 2)
          (0 2))]
    ))

(define (piece-color piece)
  (match piece
    ['s "gold"]
    ['i "teal"]
    ['z "maroon"]
    ['n "darkgreen"]
    ['t "purple"]
    ['j "blue"]
    ['l "orange"]))
    
(define (draw-cell dc x y color)
  (send dc set-brush color 'solid)
  (send dc draw-rectangle
        (+ (* x cell-size) 0)
        (+ (* y cell-size) 0)
        cell-size
        cell-size))

(define (draw-piece dc piece x y)
  (let ([color (piece-color piece)])
    (map (lambda (p) (draw-cell dc (+ x (car p)) (+ y (car (cdr p))) color))
         (piece-geometry piece))))

(define (init-grid-transform dc w h)
  (send dc set-scale 1 -1)
  (send dc translate (- (/ w 4) cell-size) (- cell-size h)))

(define (my-draw dc w h)
  (let* ([transform (send dc get-transformation)])
    (init-grid-transform dc w h)
    (draw-piece dc 's 0 0)
    (draw-piece dc 'i 3 0)
    (draw-piece dc 'z 5 0)
    (draw-piece dc 'n 9 0)
    (draw-piece dc 't 13 0)
    (draw-piece dc 'j 17 0)
    (draw-piece dc 'l 20 0)


(send dc set-transformation transform)))

(run-app (lambda (dc w h) (my-draw dc w h)) (make-color 50 50 100 1))
