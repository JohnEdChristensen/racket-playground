#lang typed/racket
(require racket/flonum)
(require math/flonum)
(struct vec ([x : Float] [y : Float]) #:transparent)
(struct vec3 ([x : Float] [y : Float] [z : Float]) #:transparent)

(: vec3-number (-> Real Real Real vec3))
(define (vec3-number x y z)
  (vec3 (fl x) (fl y) (fl z)))

(: rgb-u32 (-> Integer Integer Integer Integer))
(define (rgb-u32 r g b)
  ( + (arithmetic-shift r 16)
      (arithmetic-shift g 8)
      b))

(: v+ (-> vec vec vec))
(define (v+ v1 v2)
  (vec (+ (vec-x v1)
          (vec-x v2))
       (+ (vec-y v1)
          (vec-y v2))))

(: v3+ (-> vec3 vec3 vec3))
(define (v3+ v1 v2)
  (vec3 (+ (vec3-x v1)
          (vec3-x v2))
       (+ (vec3-y v1)
          (vec3-y v2))
       (+ (vec3-z v1)
          (vec3-z v2))
       ))

(: v* (-> vec vec vec))
(define (v* v1 v2)
  (vec (* (vec-x v1)
          (vec-x v2))
       (* (vec-y v1)
          (vec-y v2))))

(: v- (-> vec vec vec))
(define (v- v1 v2)
  (vec (- (vec-x v1)
          (vec-x v2))
       (- (vec-y v1)
          (vec-y v2))))

(: v3- (-> vec3 vec3 vec3))
(define (v3- v1 v2)
  (vec3 (- (vec3-x v1)
          (vec3-x v2))
       (- (vec3-y v1)
          (vec3-y v2))
       (- (vec3-z v1)
          (vec3-z v2))
       ))

(: vs (-> Float vec vec))
(define (vs s v)
  (vec (* s (vec-x v))
       (* s (vec-y v))))

(: v3s (-> Float vec3 vec3))
(define (v3s s v)
  (vec3 (*  s (vec3-x v))
        (*  s (vec3-y v))
        (*  s (vec3-z v))))
        
(: vabs (-> vec vec))
(define (vabs v)
  (vec (abs (vec-x v))
       (abs (vec-y v))))

(: v3abs (-> vec3 vec3))
(define (v3abs v)
  (vec3 (abs  (vec3-x v))
        (abs  (vec3-y v))
        (abs  (vec3-z v))))


(: vbetween (-> vec vec vec))
(define (vbetween a b)
  (vs 0.5 (v+ a b)))

(: dot (-> vec vec Float))
(define (dot v1 v2)
  (+ (* (vec-x v1)
        (vec-x v2))
     (* (vec-y v1)
        (vec-y v2))))

(: v3dot (-> vec3 vec3 Float))
(define (v3dot v1 v2)
  (+ (* (vec3-x v1)
        (vec3-x v2))
     (* (vec3-y v1)
        (vec3-y v2))
     (* (vec3-z v1)
        (vec3-z v2))
     ))

(: vrot (-> vec Float vec))
(define (vrot p theta)
  (vec (dot (vec (cos theta) (sin theta)) p)
       (dot (vec (- (sin theta)) (cos theta)) p)))

          
(: square (-> Float Float))
(define (square x)
  (* x x))

(: mag (-> vec Float))
(define (mag v)
  (flsqrt (+ (square (vec-x v))
             (square (vec-y v)))))

(: v3magnitude (-> vec3 Float))
(define (v3magnitude v)
  (flsqrt (+ (square (vec3-x v))
             (square (vec3-y v))
             (square (vec3-z v))
             )))

(: norm (-> vec vec))
(define (norm v)
  (vs (/ 1 (mag v)) v))

(: v3norm (-> vec3 vec3))
(define (v3norm v)
  (v3s (/ 1 (v3magnitude v)) v))

(struct segment ([start : vec] [stop : vec]) #:transparent)

(: segment-normal (-> segment vec))
(define (segment-normal s)
  (let (
        [dx (- (vec-x (segment-stop s))
                 (vec-x (segment-start s)))]
        [dy (- (vec-y (segment-stop s))
                 (vec-y (segment-start s)))]
        )
    (norm (vec (- dy) dx))))


; Fast line segment intersection
; Returns the intersection point if it exists, #f otherwise
; See Garphic gems III Chapter IV.6
(: segment-intersects (-> segment segment (U vec Boolean)))
(define (segment-intersects s1 s2)
  (let* (
        [p1 (segment-start s1)]
        [p2 (segment-stop s1)]
        [p3 (segment-start s2)]
        [p4 (segment-stop s2)]
        [A (v- p2 p1)]
        [B (v- p3 p4)]
        [C (v- p1 p3)]
        [denom  (- (* (vec-y A) (vec-x B))
                  (* (vec-x A) (vec-y B)))]
        [alpha-num (- (* (vec-y B) (vec-x C))
                      (* (vec-x B) (vec-y C)))]
        [beta-num (- (* (vec-x A) (vec-y C))
                     (* (vec-y A) (vec-x C)))])


        (: single-intersect (-> Float Float Boolean))    
        (define (single-intersect num denom)
            (if (positive? denom)
                (if (or (negative? num) (> num denom))
                    #f
                    #t
                    )
                (if (or (positive? num) (< num denom))
                    #f
                    #t)))
        (if (and (single-intersect alpha-num denom)
                 (single-intersect beta-num denom))
            ;intersection point
            (v+ p1
                (vs (/ alpha-num denom)
                    (v- p2 p1)))
            #f)))
            
(provide rgb-u32
         vec
         vec-x
         vec-y
         v+
         v*
         v-
         vs
         vbetween
         dot
         vabs
         vrot
         mag
         norm
         square

         (rename-out (vec3-number vec3))
         vec3-x
         vec3-y
         vec3-z
         v3+
         v3-
         v3s
         v3dot
         v3abs
         v3magnitude
         v3norm

         segment
         segment-start
         segment-stop
         segment-intersects
         segment-normal
         )
