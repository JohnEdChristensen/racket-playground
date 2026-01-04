#lang racket

(define (remains x)
  (remainder (* 3 x) 10))

(remains 202) ; 6
(remains 99) ; 7
(remains 79) ; 7
(remains 77) ; 1
(remains 72) ; 6
(remains 74) ; 2
(remains 82) ; 6
(remains 195) ; 5
(remains 205) ; 5



(remains 5) ; 5
(remains 7) ; 1
(remains 9) ; 7

(remains 87) ; 1
(remains 107)  ; 1
(remains 117)  ; 1
