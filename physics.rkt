#lang racket
(require "geom.rkt")

(struct particle (pos vel) #:transparent #:mutable)

(define (ray-reflection ri n)
  (v- ri (vs (* 2 (dot ri n))  n)))

; Reflect particle `p` if it intersects with the segment `obstacle`
; return false otherwise
(define (reflect past-pos p obstacle)
  (let ([intersection (segment-intersects
                       obstacle
                       (segment past-pos (particle-pos p)))])
    (if intersection
        (let ([reflected-vel (ray-reflection
                              (particle-vel p)
                              (segment-normal obstacle))])
          (particle (v+ intersection
                        (vs 0.01 reflected-vel))
                    reflected-vel)
          )
        #f)))

(define (spring-force p1 p2 k rest-length)
  (let* ([p1-to-p2 (v- p2 p1)]
         [l (magnitude p1-to-p2)]
         [dl (- rest-length l)]
         )
    (vs (* k dl) (norm p1-to-p2))
    ))

(define (repulsive-force p1 p2 s)
  (let* ([p1-to-p2 (v- p2 p1)]
         [f (/ s (square (magnitude p1-to-p2)))]
         )
    (vs f (norm p1-to-p2))))


(provide particle
         particle-pos
         particle-vel
         reflect
         spring-force
         repulsive-force
         )


