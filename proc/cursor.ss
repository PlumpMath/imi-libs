(library (imi proc cursor)
  (export do-times)
  (import (rnrs)
          (imi utils math curried-operator))

  ;;; calls a procedure `n` times, optionally
  ;;;  with an argument, which will be passed
  ;;;  to proc and the result next time applied
  ;;;  to proc
  ;;;
  ;;; n - integer?
  ;;; proc - (-> any?)
  ;;;  -> #f
  ;;; --
  ;;; n - integer?
  ;;; proc - (-> argtype argtype)
  ;;; x - argtype
  ;;;  -> argtype
  (define do-times
    (case-lambda
      [(n proc)
       (and (> n 0)
            (proc)
            (do-times (sub1 n) proc))]
      [(n proc x)
       (if (zero? n)
         x
         (do-times (sub1 n)
                   proc
                   (proc x)))]))

  )
