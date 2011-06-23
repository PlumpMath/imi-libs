#!r6rs

(import (rnrs)
        (ikarus)
        (gl)
        (glu)
        (freeglut))

(define (parameter-caller para)
  (lambda args
    ;(printf "called parameter func ~s~%" para)
    (apply (para) args)))

(define (parameter-caller* para)
  (lambda args
    (apply (para) args)))

(define GLDisplayFunc
  (make-parameter
    (lambda args #f)))

(define GLIdleFunc
  (make-parameter
    (lambda args #f)))

(define GLReshapeFunc
  (make-parameter
    (lambda args #f)))

(define GLKeyboardFunc
  (make-parameter
    (lambda args #f)))

(define gldisplayer
  (parameter-caller GLDisplayFunc))

(define glidler
  (parameter-caller* GLIdleFunc))

(define glreshaper
  (parameter-caller GLReshapeFunc))

(define glkeyboarder
  (parameter-caller GLKeyboardFunc))

(define (glutInitProcs)
  (glutDisplayFunc gldisplayer)
  (glutIdleFunc glidler)
  (glutReshapeFunc glreshaper)
  (glutKeyboardFunc glkeyboarder))

(define (glutMainInit args)
  (let ([argv (list->vector args)])
    (glutInit (vector (vector-length argv))
              argv))

  (glutInitDisplayMode
    (+ GLUT_RGBA
       GLUT_DOUBLE
       GLUT_ALPHA
       GLUT_DEPTH)))

(define (glutSimpleInit width height)
  (glutMainInit '())
  (glutInitWindowSize width height)
  (glutInitWindowPosition 100 50))

(define (glSimpleInit width height)
  (glClearColor .0 .0 .0 1.0)
  (glClearDepth 1.0)
  (glDepthFunc GL_LESS)
  (glEnable GL_DEPTH_TEST)
  (glShadeModel GL_SMOOTH)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective 45.0
                  (inexact (/ width height))
                  0.1
                  100.0)
  (glMatrixMode GL_MODELVIEW))

(define rnrs/ikarus/gl/glut-env
  (environment '(rnrs)
               '(ikarus)
               '(gl)
               '(glut)))

(define (read-eval . args)
  (let loop ([in (read)])
    (when in
      (eval `(let ([args ,args])
               ,in)
            rnrs/ikarus/gl/glut-env)
      (loop (read)))))

(define-syntax gl-do
  (syntax-rules ()
    [(_ what body ...)
     (begin (glBegin what)
            body ...
            (glEnd))]))

(define rot 0.0)

(define (draw-pyramid)
  (gl-do GL_TRIANGLES
    (glColor3f 1.0 0.0 0.0)
    (glVertex3f 0.0 1.0 0.0)
    (glColor3f 0.0 1.0 0.0)
    (glVertex3f -1.0 -1.0 1.0)
    (glColor3f 0.0 0.0 1.0)
    (glVertex3f 1.0 -1.0 1.0))
  (gl-do GL_TRIANGLES
    (glColor3f 1.0 0.0 0.0)
    (glVertex3f 0.0 1.0 0.0)
    (glColor3f 0.0 1.0 0.0)
    (glVertex3f -1.0 -1.0 1.0)
    (glColor3f 0.0 0.0 1.0)
    (glVertex3f -1.0 -1.0 -1.0))
  (gl-do GL_TRIANGLES
    (glColor3f 1.0 0.0 0.0)
    (glVertex3f 0.0 1.0 0.0)
    (glColor3f 0.0 1.0 0.0)
    (glVertex3f 1.0 -1.0 -1.0)
    (glColor3f 0.0 0.0 1.0)
    (glVertex3f -1.0 -1.0 -1.0))
  (gl-do GL_TRIANGLES
    (glColor3f 1.0 0.0 0.0)
    (glVertex3f 0.0 1.0 0.0)
    (glColor3f 0.0 1.0 0.0)
    (glVertex3f 1.0 -1.0 -1.0)
    (glColor3f 0.0 0.0 1.0)
    (glVertex3f 1.0 -1.0 1.0)))

(define (draw-polygon)
  (glClear (+ GL_COLOR_BUFFER_BIT
              GL_DEPTH_BUFFER_BIT))
  (glLoadIdentity)
  (glTranslatef -1.5 0.0 -6.0)
  (glRotatef rot 0.0 1.0 0.0)
  (set! rot (+ rot 2))
  (draw-pyramid)
  #;
  (gl-do GL_TRIANGLES
    (glColor3f 1.0 0.0 0.0)
    (glVertex3f 0.0 1.0 0.0)
    (glColor3f 0.0 1.0 0.0)
    (glVertex3f -1.0 -1.0 0.0)
    (glColor3f 0.0 0.0 1.0)
    (glVertex3f 1.0 -1.0 0.0))
  (glLoadIdentity)
  (glTranslatef 1.5 0.0 -6.0)
  (glColor3f 0.5 0.5 1.0)
  (gl-do GL_QUADS
    (glVertex3f -1.0 1.0 0.0)
    (glVertex3f 1.0 1.0 -1.0)
    (glVertex3f 1.0 -1.0 -2.0)
    (glVertex3f -1.0 -1.0 -1.0))
  (glutSwapBuffers))

(define (simple-resize width height)
  (cond
    [(zero? height)
     (simple-resize width 1)]
    [else
     (glViewport 0 0 width height)
     (glMatrixMode GL_PROJECTION)
     (glLoadIdentity)
     (gluPerspective 45.0
                     (inexact (/ width height))
                     0.1
                     100.0)
     (glMatrixMode GL_MODELVIEW)
     (draw-polygon)]))

(define (close-func win)
  (lambda (key x y)
    (case key
      [(27) ;escape
       (glutLeaveMainLoop)]
      [(17) ;ctrl-q
       (glutDestroyWindow win)]
      [else
        (printf "pressed key ~s at (~s ~s)~%" key x y)
        #f])))
      

(define (ogl-simple-test)
  (glutSimpleInit 500 400)
  (let ([win (glutCreateWindow
               "Simple Test Window")])
    (glutInitProcs)
    (parameterize ([GLDisplayFunc draw-polygon]
                   [GLIdleFunc draw-polygon]
                   [GLReshapeFunc simple-resize]
                   [GLKeyboardFunc (close-func win)])
      (glSimpleInit 500 400)
      (glutMainLoop)
      win)))

(define win (ogl-simple-test))
