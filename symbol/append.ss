#!r6rs

(library (imi symbol append)
  (export symbol-append)
  (import (rnrs))

  (define (symbol-append . sym)
    (string->symbol
      (apply string-append
             (map symbol->string sym))))

  )
