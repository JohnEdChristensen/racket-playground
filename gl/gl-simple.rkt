#lang racket
(require mred
         sgl/gl
         sgl/gl-vectors
         opengl/util
         (only-in opengl glUseProgram)
         "gl-frame.rkt"
         "../watch.rkt")

(define *reload-shaders* #f)
(watch "./a.frag" (lambda () (set! *reload-shaders* #t)))

(define (refresh-shaders)
  (glUseProgram (create-program (load-shader "./a.vert" 35633) (load-shader "./a.frag" 35632)))
  (set! *reload-shaders* #f))

;; Init function
(define (my-gl-init)
  ;; Set-up alpha blending 50% transparency
  (glColor4d 1 1 1 0.5)
  (glBlendFunc GL_SRC_ALPHA GL_ONE)
  (glEnable GL_BLEND)

  (refresh-shaders)
  ;; Standard Init
  (glEnable GL_TEXTURE_2D)
  (glShadeModel GL_SMOOTH)
  (glClearColor 0.0 0.0 0.0 0.5)
  (glClearDepth 1)
  (glEnable GL_DEPTH_TEST)
  (glDepthFunc GL_LEQUAL)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)

  ;; default light
  (glEnable GL_LIGHT0)
  (glEnable GL_LIGHTING))

(define (my-gl-draw)
  ;(refresh-shaders)

  (if *reload-shaders*
      (refresh-shaders)
      null)
  ;; erase the background
  (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  ;; draw cube.
  (glLoadIdentity)
  (glBegin GL_TRIANGLES)
  (glVertex3i -1 1 0)
  (glVertex3i 1 1 0)
  (glVertex3i -1 -1 0)
  (glVertex3i -1 -1 0)
  (glVertex3i 1 1 0)
  (glVertex3i 1 -1 0)

  (glEnd)
  (glFlush))

;; Set the init function
(set-gl-init-fn my-gl-init)
;; Set the draw function
(set-gl-draw-fn (thunk (my-gl-draw)))
(gl-run)
