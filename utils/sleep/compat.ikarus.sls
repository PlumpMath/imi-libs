#!r6rs

(library (imi utils sleep compat)
  (export sleep)
  (import (only (racket)
                sleep))

  (define (sleep n)
    (nanosleep
      (exact (floor n))
      (exact
        (floor
          (* (- n (floor n))
             1000      ;milli
             1000      ;micro
             1000))))) ;nano

  )
