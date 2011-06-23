#!r6rs

(library (imi asm arm opcodes register)
  (export register?)
  (import (rnrs))

  (define (register? x)
    (and (integer? x)
         (<= 0 x 15)))

  )

