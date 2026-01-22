#lang racket
(require racket/draw)
(provide draw-l-system)

(define (draw-step dc l)
  (cond [(eq? l 'F)
         (send dc draw-line 0 0 10 0)
         (send dc translate 10 0 )]
        [(eq? l '+) (send dc rotate (/ pi 2))]
        [(eq? l '-) (send dc rotate (/ pi -2))]
        ))

(define (draw-l-system dc ls)
  (for-each (lambda (l) (draw-step dc l))
            ls))
