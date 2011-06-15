(library (imi random proc)
  (export random-boolean
          curry-random-real)
  (import (rnrs)
          (imi random number))

  ;;; returns randomly a boolean
  ;;;
  ;;;  -> boolean?
  (define (random-boolean)
    (zero? (random-integer 2)))

  ;;; returns a procedure which
  ;;;  calls `proc` with a random
  ;;;  real between 0 and 1
  ;;;
  ;;; `proc` - (-> rettype
  ;;;              (number-between/c 0 1))
  ;;;  -> (-> rettype)
  (define (curry-random-real proc)
    (lambda ()
      (proc (random-real))))

  )
