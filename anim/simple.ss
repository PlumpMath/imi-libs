#!r6rs

(library (imi anim simple)
  (export animation
          anim-reverse
          anim-invert
          anim-scale
          anim-move
          anim-linear
          anim-sine)
  (import (rnrs)
          (imi math)
          (imi utils sleep))

  ;;; calls `proc` `resolution` times per second
  ;;;   `len` seconds long with the values of the
  ;;;   curve `anim`
  ;;;     `anim` will be called with a number
  ;;;       between 0 and 1 representing the procentual
  ;;;       state of the animation
  ;;; 
  ;;; proc - (-> any? pred)
  ;;; resolution - (number>/c 0)
  ;;; len - (number>=/c 0)
  ;;; anim - (-> pred (number-in/c 0 1))
  ;;;  -> void?
  (define (animation proc resolution len anim)
    (let ([sleeptime (/ resolution)]
          [steps (* resolution len)])
      (let loop ([step 1])
        (unless (> step steps)
          (proc (anim (/ step steps)))
          (sleep sleeptime)
          (loop (add1 step))))))

  
  ;;; reverses curve `anim` (x-axis)
  ;;;
  ;;; anim - (-> pred (number-in/c 0 1))
  ;;;  -> (-> pred (number-in/c 0 1))
  (define (anim-reverse anim)
    (lambda (s)
      (anim (- 1 s))))

  ;;; inverts curve `anim` (flips upside-down)
  ;;;
  ;;; anim - (-> pred (number-in/c 0 1))
  ;;;  -> (-> pred (number-in/c 0 1))
  (define (anim-invert anim)
    (lambda (s)
      (- 1 (anim s))))

  ;;; scales curve `anim` `x` on x-axis and
  ;;;   `y` on y-axis
  ;;;
  ;;; x - (number>/c 0)
  ;;; y - (number>/c 0)
  ;;; anim - (-> pred (number-in/c 0 1))
  ;;;  -> (-> pred (number-in/c 0 1))
  (define (anim-scale x y anim)
    (lambda (s)
      (* y (anim (/ s x)))))

  ;;; moves curve `anim` `x` right and
  ;;;   `y` upwards
  ;;;
  ;;; x - number?
  ;;; y - number?
  ;;; anim - (-> pred (number-in/c 0 1))
  ;;;  -> (-> pred (number-in/c 0 1))
  (define (anim-move x y anim)
    (lambda (s)
      (+ y (anim (- s x)))))

  ;;; linear curve from 0 to 1
  ;;;
  ;;; s - (number-in/c 0 1)
  ;;;  -> (number-in/c 0 1)
  (define (anim-linear s)
    s)

  ;;; sine curve from 0 to 1
  ;;;
  ;;; s - (number-in/c 0 1)
  ;;;  -> (number-in/c 0 1)
  (define (anim-sine s)
    (sin (* s 1/2 *pi*)))

  )
