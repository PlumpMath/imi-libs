#!r6rs

(library (imi iter pipeline-extra)
  (export pipe-getn)
  (import (rnrs)
          (imi math)
          (imi iter pipeline))

  (define (pipe-getn n)
    (if (zero? n)
        (pipeline '())
        (pipeline x <- (pipe-get)
                  rest <- (pipe-getn (sub1 n))
                  (cons x rest))))

  )
