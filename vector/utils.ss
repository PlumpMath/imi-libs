#!r6rs

(library (imi vector utils)
  (export vector-last-pos)
  (import (rnrs))

  (define (vector-last-pos vec)
    (- (vector-length vec) 1))

  )
