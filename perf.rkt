#lang typed/racket

(require math/statistics)

(: timeit (-> (-> Any) Float))
(define (timeit p)
  (let ([start (current-inexact-milliseconds)])
    (p)
    (- (current-inexact-milliseconds) start)))

(: bench (-> Integer (-> Any) Float))
(define (bench n p)
  (median < (map (lambda (i) (timeit p)) (range n))))


(provide timeit
         bench)
