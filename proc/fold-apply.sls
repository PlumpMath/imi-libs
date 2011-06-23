#!r6rs

(library (imi proc fold-apply)
  (export fold-apply
          fold-apply1
          fold-apply2)
  (import (rnrs))

  (define (fold-apply proc ls)
    (fold-left proc (proc) ls))

  (define (fold-apply1 proc ls)
    (fold-left proc
               (proc (car ls))
               (cdr ls)))

  (define (fold-apply2 proc ls)
    (fold-left proc
               (car ls)
               (cdr ls)))
  
  )

