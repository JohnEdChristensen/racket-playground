#lang typed/racket

(require math/flonum)

(require "geom.rkt")
(require "perf.rkt")
(provide sdf-color
         circle-sdf
         box-sdf
         ring-sdf
         sdf-g
         sdf-e
         sdf-n
         sdf-u
         sdf-a
         sdf-r
         sdf-y
         )

(: sdf-color (-> Float Integer))
(define (sdf-color v)
  (let ([phase (+ 0.4 (* 0.3 ( + 1 (cos (* 50 v)))))])
    (cond ((negative? v) (rgb-u32
                          (exact-round (* phase 180))
                          (exact-round (* phase 100))
                          (exact-round (* phase 240))))
          (else (rgb-u32 (exact-round (* phase 100))
                          (exact-round (* phase 200))
                          (exact-round (* phase 220)))))))

(: circle-sdf (-> vec Float Float))
(define (circle-sdf p r)
  (- (magnitude p) r))

;float sdBox( in vec2 p, in vec2 b )
;{
;    vec2 d = abs(p)-b;
;    return length(max(d,0.0)) + min(max(d.x,d.y),0.0);
;}

(: box-sdf (-> vec vec Float))
(define (box-sdf p b)
  (let* ([d (v- (vabs p) b)])
    (+ (magnitude (vec (max (vec-x d) 0.0)
                       (max (vec-y d) 0.0)))
       (min (max (vec-x d) (vec-y d))
            0))))

(: ring-sdf (-> vec vec Float Float Float))
(define (ring-sdf p n r th)
  (let* ([np (vec (abs (vec-x p)) (vec-y p))]
        [mpx  (dot (vec (vec-x n) (- (vec-y n))) np)]
        [mpy  (dot (vec (vec-y n) (vec-x n)) np)]
        [mp (vec mpx mpy)]
        [a (- (abs (- (magnitude mp) r))
              (* 0.5 th))]
        [b (* (magnitude (vec (vec-x mp)
                              (max 0.0 (- (abs (- r
                                                (vec-y mp)))
                                        (* 0.5 th)))))
              (sgn (vec-x mp)))])
    (max a b)

    ))


;float sdRing( in vec2 p, in vec2 n, in float r, float th )
;{
;    p.x = abs(p.x);
;    p = mat2x2(n.x,n.y,-n.y,n.x)*p;
;    return max( abs(length(p)-r)-th*0.5,
;                length(vec2(p.x,max(0.0,abs(r-p.y)-th*0.5)))*sign(p.x) );
;}
; (circle-sdf (v- p (vec -0.2 0.5)) 0.2)))))


(define font-rad 0.10)
(define hfr (/ font-rad 2.0))
(define font-width (/ font-rad 3.0))
(define hfw (/ font-width 2.0))

(: sdf-tail (-> vec Float))
(define (sdf-tail p)
       (min (box-sdf (v- p (vec font-rad font-rad))
                (vec hfw  font-rad))
       (ring-sdf (v- p (vec 0.0 (* 2.0 font-rad)))
                 (vec 0.0 1.0)
                 font-rad 
                 font-width)))
  
(: sdf-g (-> vec Float))
(define (sdf-g p)
  (min (circle-sdf p
                   (+ font-rad hfw))
       (sdf-tail p)))

(: sdf-e (-> vec Float))
(define (sdf-e p)
  (min (box-sdf (v- p (vec hfr 0.0))
                (vec hfr (/ font-width 3.0)))
       (ring-sdf (vrot p (* 3.0 (/ pi 4.0)))
                 (norm (vec -1.0 1.0))
                 (- font-rad hfw)
                 font-width)))

(: sdf-n (-> vec Float))
(define (sdf-n p)
  (min (ring-sdf (vrot p pi)
                 (norm (vec 0.0 1.0))
                 (- font-rad hfw)
                 font-width)
       (box-sdf (v- p (vec (- font-rad hfw) hfr))
                (vec hfw (+ 0.01 hfr) ))
       (box-sdf (v- p (vec (+ (- font-rad) hfw) hfr))
                (vec hfw (+ 0.01 hfr) ))
       ))

(: sdf-u (-> vec Float))
(define (sdf-u p)
  (sdf-n (vrot (v- p (vec 0.0 hfw)) pi)))

(: sdf-a (-> vec Float))
(define (sdf-a p)
  (min (box-sdf (v- p (vec (- font-rad hfw) hfr))
                (vec (/ font-width 2.0) hfr))
       (ring-sdf (vrot p (* 3.0 (/ pi 4.0)))
                 (vec -1.0 0.0)
                 (- font-rad hfw)
                 font-width)))

(: sdf-r (-> vec Float))
(define (sdf-r p)
  (min (ring-sdf (vrot p pi)
                 (norm (vec 0.0 1.0))
                 (- font-rad hfw)
                 font-width)
       (box-sdf (v- p (vec (+ (- font-rad) hfw) hfr))
                (vec hfw (+ 0.01 hfr) ))
       ))

(: sdf-y (-> vec Float))
(define (sdf-y p)
  (min (sdf-u p)
       (sdf-tail (v- p (vec (- hfw) 0.0)))))

