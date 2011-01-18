(library (imi list utils)
  (export length<)
  (import (rnrs))

  ;;; checks if `(length ls)` is smaller than
  ;;;   `maxlen` but much faster for small
  ;;;   `maxlen` and large `ls`
  ;;;
  ;;; ls - list?
  ;;; maxlen - positive-integer?
  ;;;  -> boolean?
  (define (length< ls maxlen)
    (cond
      [(zero? maxlen) #f]
      [(null? ls) #t]
      [else (length< (cdr ls) (sub1 maxlen))]))

  )
