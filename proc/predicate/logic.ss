#!r6rs

(library (imi proc predicate logic)
  (export or/p
          and/p)
  (import (rnrs)
          (imi deprecated))

  ;;; DRAFTS

  ;;; slow implementation? better with (let loop ...)

  (define (or/p pred0 . rest)
    (if (null? rest)
      pred0
      (lambda (x)
        (or (pred0 x)
            ((apply or/p rest) x)))))

  (define (and/p pred0 . rest)
    (if (null? rest)
      pred0
      (lambda (x)
        (and (pred0 x)
             ((apply and/p rest) x)))))

  (mark-deprecated)

  )
