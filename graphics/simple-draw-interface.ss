#!r6rs

(library (imi graphics simple-draw-interface)
  (export make-simple-draw-interface
          simple-draw-interface?
          move-area
          draw-pixel
          get-pixel
          draw-line
          draw-rect
          draw-filled-rect
          draw-circle
          draw-filled-circle
          make-subdrawer
          
          make-point
          point-x
          point-y

          make-rect
          rect-left
          rect-top
          rect-right
          rect-bottom
          
          make-color
          color-r
          color-g
          color-b)
  (import (rnrs)
          (imi deprecated))

  (define-record-type simple-draw-interface
    (fields custom-data
            area-mover
            pixel-drawer
            pixel-getter
            line-drawer
            rect-drawer
            filled-rect-drawer
            circle-drawer
            filled-circle-drawer
            subdrawer-creator))

  (define (move-area sdi rect directions)
    ((simple-draw-interface-area-mover sdi)
     (simple-draw-interface-custom-data sdi)
     rect directions))

  (define (draw-pixel sdi point color)
    ((simple-draw-interface-pixel-drawer sdi)
     (simple-draw-interface-custom-data sdi)
     point color))

  (define (get-pixel sdi point)
    ((simple-draw-interface-pixel-getter sdi)
     (simple-draw-interface-custom-data sdi)
     point))

  (define (draw-line sdi from to color)
    ((simple-draw-interface-line-drawer sdi)
     (simple-draw-interface-custom-data sdi)
     from to color))

  (define (draw-rect sdi rect color)
    ((simple-draw-interface-rect-drawer sdi)
     (simple-draw-interface-custom-data sdi)
     rect color))

  (define (draw-filled-rect sdi rect color)
    ((simple-draw-interface-filled-rect-drawer sdi)
     (simple-draw-interface-custom-data sdi)
     rect color))

  (define (draw-circle sdi rect color)
    ((simple-draw-interface-circle-drawer sdi)
     (simple-draw-interface-custom-data sdi)
     rect color))

  (define (draw-filled-circle sdi rect color)
    ((simple-draw-interface-filled-circle-drawer sdi)
     (simple-draw-interface-custom-data sdi)
     rect color))

  (define (make-subdrawer sdi rect)
    ((simple-draw-interface-subdrawer-creator sdi)
     (simple-draw-interface-custom-data sdi)
     sdi
     rect))


  (define-record-type point (fields x y))

  (define-record-type rect (fields left top right bottom)
    (protocol
      (lambda (new)
        (case-lambda
          [(pt0 pt1) (new (pos-x pt0) (pos-y pt0)
                          (pos-x pt1) (pos-y pt1))]
          [(l t r b) (new l t r b)]))))

  (define-record-type color (fields r g b))

  )
