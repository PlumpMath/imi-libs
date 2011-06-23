#!r6rs

(library (imi labs psys)
  (export rcall)
  (import (rnrs))

  (define (rcall proc arg . rest)
    (if (null? rest)
        (proc arg)
        (apply rcall (proc arg) rest)))

  )

