(library (imi iter extra)
  (export limit-iter)
  (import (rnrs)
          (imi iter iterator)
          (imi sugar for))

  ;;; limits iterator `iter` by another
  ;;;   iterator `lim-iter`, generates a
  ;;;   new iterator, which returns values
  ;;;   from `iter` and ends when either
  ;;;   `lim-iter` or `iter` ends
  ;;;
  ;;; iter - (iterator/c val)
  ;;; lim-iter - iterator?
  ;;;  -> (iterator/c val)
  (define (limit-iter iter lim-iter)
    (for loop ([limiter in lim-iter]
               [value in iter])
         #f
      (make-iterator value loop)))

  )
