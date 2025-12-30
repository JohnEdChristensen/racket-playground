#lang racket
(require racket/gui)

;(define (draw dc size) 'nil)
(define (run-app draw color)
  (define frame (new frame%
                     [label "Example"]
                     [width 300]
                     [height 300]))


(define (paint-canvas canvas dc)
   (let-values ([(w h) (send canvas get-client-size)])
     (draw dc w h)))
  
  (define my-canvas (new canvas% [parent frame]
                         [paint-callback paint-canvas]
                         ))

  (new timer% [notify-callback
               (lambda ()
                 (send my-canvas refresh-now
                       (lambda (dc)
                         (paint-canvas my-canvas dc))))]
                        
       [interval 16])
  (send my-canvas set-canvas-background color)
  (send frame show #t)
  
frame
)
(provide run-app)






