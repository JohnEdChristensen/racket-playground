#lang racket
(require racket/gui)

(define run-app 
  (lambda (draw color [dir "default"] [frames 0] [dt 0.016] [w 512] [h 512])
    (define running-timer null)
    (define frame (new (class frame%
                         (super-new)
                         (define/augment (on-close)
                           (send running-timer stop)))
                     [label dir]
                     [width w]
                     [height h]))

  (define (now) (* 0.001 (current-milliseconds)))
  (define start-time (now))
  (define (elapsed-time) (- (now) start-time))

  (define current-frame 0)

  (define (paint-canvas canvas dc)
     (let-values ([(w h) (send canvas get-client-size)])
       (draw dc w h (elapsed-time))))

  (define (save-frame i dt)
   (define my-bitmap (send my-canvas make-bitmap w h))
                 (draw (send my-bitmap make-dc) w h  (* i dt))
                 (send my-bitmap save-file 
                       (~a "./anim/" dir "/" i ".png")  
                       'png))
  
  (define my-canvas (new canvas% [parent frame]
                         [paint-callback paint-canvas]
                         ))
  (send my-canvas set-canvas-background color)
  (send frame show #t)

  (set! running-timer (new timer% 
       [notify-callback 
         (lambda () 
           (set! current-frame (+ current-frame 1))
           (send my-canvas refresh-now
              (lambda (dc)
                 (paint-canvas my-canvas dc)))
           (if (< current-frame frames)
               (save-frame current-frame dt)
               '()
               ))]
       [interval 16]))
  frame
))
;;
;; (define (save-anim draw color dir)
;;   (define frame (new frame%
;;                      [label "Example"]
;;                      [width 300]
;;                      [height 300]))
;;
;;
;;   (define (paint-canvas canvas dc)
;;    (let-values ([(w h) (send canvas get-client-size)])
;;      (draw dc w h (* 0.001 (current-milliseconds)))))
;;
;;   (define my-canvas (new canvas% [parent frame]
;;                          [paint-callback paint-canvas]
;;                          ))
;;   (send my-canvas set-canvas-background color)
;;   (send frame show #t)
;;
;;
;;   (for ([i (in-range 200)])
;;                  (send my-canvas refresh-now
;;                        (lambda (dc)
;;                          (paint-canvas my-canvas dc)))
;;
;;
(provide run-app)
;; (provide save-anim)






