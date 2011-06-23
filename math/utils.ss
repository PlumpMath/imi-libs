#!r6rs

(library (imi math utils)
  (export most)
  (import (rnrs)
          (imi list utils))

  (define (most more? ls)
    (cond
      [(length=? ls 1) (car ls)]
      [else
        (let-values ([(left right) (halves ls)])
          (let ([lmax (and (not (null? left))
                           (most more? left))]
                [rmax (most more? right)])
            (if (and lmax (more? lmax rmax))
              lmax
              rmax)))]))

  )
