#lang racket
(provide step)

(define (step rules vals n)
  (if (= n 0)
      vals
      (step rules (flatten (map rules vals)) (- n 1))))

(define a-variables '(A B))
(define a-axiom '(A))
(define (a-rules X)
  (cond [(eq? X 'A) '(A B)]
        [(eq? X 'B) '(A)]))
(step a-rules a-axiom 4)
