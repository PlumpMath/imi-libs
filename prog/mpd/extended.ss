#!r6rs

(library (imi prog mpd extended)
  (export mpc-fade-out
          mpc-fade-in
          mpc-fade-vol)
  (import (rnrs)
          (imi prog mpd simple-control)
          (imi anim simple))

  (define (mpc-fade-vol time endvol)
    (let ([startvol (cdr (assq 'volume (mpc-status)))])
      (animation
        (lambda (s) (mpc-setvol (exact (round s))))
        (/ 100 time)
        time
        (anim-scale 1 (- endvol startvol)
                    (anim-move 0 startvol
                               anim-sine)))))

  (define (mpc-fade-out time)
    (let ([volume (cdr (assq 'volume (mpc-status)))])
      (animation
        (lambda (s) (mpc-setvol (exact (floor s))))
        (/ 100 time)
        time
        (anim-scale 1 volume
                    (anim-reverse
                      anim-sine)))
      (mpc-pause)
      (mpc-setvol volume)))

  (define (mpc-fade-in time volume)
    (mpc-setvol 0)
    (mpc-play)
    (animation
      (lambda (s) (mpc-setvol (exact (floor s))))
      (/ 100 time)
      time
      (anim-scale 1 volume
                  (anim-invert
                    (anim-reverse
                      anim-sine)))))

  )
