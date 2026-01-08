#lang racket
(require racket/draw)
(require math)
(require "run.rkt")
(require "geom.rkt")
(require "shapes.rkt")
(require "physics.rkt")

(define bg (make-color 240 220 180 1))
(define bg2 (make-color 100 100 100 1))
(define fg (make-color 250 100 200 1))

(define dt 0.0015)
(define k 80.0)
(define s 1.0)
(define default-length 10.0)
(define corner-length (* default-length (sqrt 2)))

(define obstacles (list (segment (vec -100. 100.) (vec -90. 0.))
                        (segment (vec -90. 0.) (vec -70. -50.))
                        (segment (vec -70. -50.) (vec -30. -75.))
                        (segment (vec -30. -75.) (vec 0. -75.))
                        (segment (vec 0. -75.) (vec 20. -65.))
                        (segment (vec 20. -65.) (vec 30. -55.))

                        (segment (vec 100. -65.) (vec 100. -100.))
                        (segment (vec 100. -100.) (vec 140. -100.))
                        (segment (vec 140. -100.) (vec 140. -65.))
                        ;(segment (vec -100 85) (vec 100 85))
                        ))

(define particles (map (lambda (index)
                         (let (
                               [x (+ -95.1 (* (car index) default-length))]
                               [y (+ 250.1 (* (car (cdr index)) default-length))])
                         (particle (vec x y) (vec 0. -60.))
                         ))
                       (list (list 0 0)
                             (list 0 1)
                             (list 0 2)
                             (list 1 0)
                             (list 1 1)
                             (list 1 2)
                             (list 2 0)
                             (list 2 1)
                             (list 2 2))))

(struct spring (i1 i2 rest-length) #:transparent)

(define springs (list
  (spring 0 1 default-length)
  (spring 0 3 default-length)
  (spring 0 4 corner-length)

  (spring 1 2 default-length)
  (spring 1 4 default-length)
  (spring 1 5 corner-length)
  (spring 1 3 (* 1.0 corner-length))

  (spring 2 5 default-length)
  (spring 2 4 (* 1.0 corner-length))

  (spring 3 4 default-length)
  (spring 3 6 default-length)
  (spring 3 7 corner-length)

  (spring 4 5 default-length)
  (spring 4 7 default-length)
  (spring 4 8 corner-length)
  (spring 4 6 (* 1.0 corner-length))

  (spring 5 8 default-length)
  (spring 5 7 (* 1.0 corner-length))

  (spring 6 7 default-length)
  (spring 7 8 default-length)
  ))

(define (attached-springs i)
  (filter (lambda (s)
            (or (= i (spring-i1 s))
                (= i (spring-i2 s))))
          springs))

(define (combined-spring-force i)
  (foldl v+
         (vec 0. 0.)
         (map (lambda (s)
                        (let* (
                               [i1 (spring-i1 s)]
                               [i2 (spring-i2 s)]
                               [pos1 (particle-pos (list-ref particles i1))]
                               [pos2 (particle-pos (list-ref particles i2))]
                               [rest-length (spring-rest-length s)])
                          (if (= i (spring-i2 s))
                              (spring-force pos1 pos2 k rest-length)
                              (spring-force pos2 pos1 k rest-length))))
         (attached-springs i))))

(define (update-particle p i obstacles)
  (let* ([vel (particle-vel p)]
         [pos (particle-pos p)]
         [f-springs (combined-spring-force i)]
         [new-vel (vs 0.9998 (v+ vel
                      (vs dt
                          (v+ (vec 0. -10.)
                              f-springs))))]
         [new-pos (v+ pos (vs dt new-vel))]
         ; return first reflected particle if one exists
         [reflected-particle (ormap (lambda (obstacle)
                         (reflect pos
                                  (particle new-pos new-vel)
                                  obstacle))
                  obstacles)])
    (if reflected-particle
        reflected-particle
        (particle new-pos new-vel))))

(define (left-particle-index i n)
   (modulo (- i 1) n))
(define (right-particle-index i n)
  (modulo (+ i 1) n))

(define (cycle-indexes n)
  (map (lambda (x) (cons x (modulo (+ x 1) n)))
       (range n)))


(define (my-draw dc w h now)
  (let* ([transform (send dc get-transformation)])

    (define n (length particles))
    (for ([i (range 50)]) (set! particles
          (map (lambda (p i)
                 (update-particle p i obstacles))
               particles
               (range n)
               )))

    (send dc set-brush bg 'solid)
    (send dc set-pen bg 2 'solid)
    (send dc draw-rectangle 0 0 w h)

    (send dc translate (- (/ w 2) 50) (/ h 2))
    (send dc scale 1 -1)

    (send dc set-pen bg 2 'solid)
    (send dc set-brush  fg 'solid)

    (for-each (lambda (p) (draw-circle dc (particle-pos p) 2 fg)) particles)
    (for-each (lambda (sp)
                (draw-segment dc
                              (segment (particle-pos (list-ref particles (spring-i1 sp)))
                                       (particle-pos (list-ref particles (spring-i2 sp))))
                              fg
                              6))
              (flatten (map attached-springs (range n))))
              

    (for-each (lambda (s) (draw-segment dc s bg2 3)) obstacles)

    (send dc set-transformation transform)))

(run-app (lambda (dc w h now) (my-draw dc w h now)) bg "26-01-02" 200)

