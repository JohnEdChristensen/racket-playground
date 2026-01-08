#lang typed/racket

(require racket/flonum)
(require math/flonum)

(require "geom.rkt")
(require "perf.rkt")


(define (bench-make-vec)
  (map (lambda ([i : Real]) (vec (fl i) (fl i))) (range 100000)))

(define vs (bench-make-vec))

(define (bench-add-vec)
  (foldl (lambda (v a) (v+ v a) ) (vec 0.0 0.0) vs))

(define (bench-mag-vec)
  (foldl (lambda ([v : vec] [a : Float]) (fl+ a (magnitude v) )) 0.0  vs))

(print "make-vec")
(println (bench 10 bench-make-vec))

(print "add-vec")
(println (bench 10 bench-add-vec))

(print "mag-vec")
(println (bench 10 bench-mag-vec))
