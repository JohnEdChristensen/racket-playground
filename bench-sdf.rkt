#lang typed/racket

(require racket/draw)
(require math/flonum)

(require "geom.rkt")
(require "perf.rkt")
(require "sdf.rkt")


(define res-x 256)
(define res-y 128)

(: bench-sdf (-> (-> vec Any) (-> Any)))
(define (bench-sdf proc)
  (lambda () 
    (for* ([i (range res-x)]
          [j (range res-y)])
      (let* (
             [x (/ (- (* 2 i) res-y) res-x)]
             [y (/ (- (* 2 j) res-y) res-x)]
             [p (vec (+ x 0.25) (+ y 0.05))])
        (proc p)
        ))))

;(define bm (make-bitmap res-x res-y #f))
;(define fast-setpixel (make-fast-setpixel bm))

;(define (compute-and-set w h)
;  (for* ([i (range w)]
;        [j (range h)])
;        (fast-setpixel i j (render i j w h))
;        ))

(define circle-bench (bench-sdf (lambda ([p : vec]) (circle-sdf p 0.5))))
(define box-bench (bench-sdf (lambda (p) (box-sdf p (vec 0.5 0.25)))))
(define ring-bench (bench-sdf (lambda (p) (ring-sdf p (vec 0.0 1.0) 0.5 0.2))))
;(define compute-and-set-bench (lambda () (compute-and-set res-x res-y)))

(define n 10)

(print "circle: ")
(print (bench 10 circle-bench))
(print "box: ")
(print (bench 10 box-bench))
(print "ring: ")
(print (bench 10 ring-bench))
;(print "compute-and-set: ")
;(print (bench 10 compute-and-set-bench))
