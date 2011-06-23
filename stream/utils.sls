#!r6rs

(library (imi stream utils)
  (export stream-group)
  (import (rnrs)
          (imi sugar receive)
          (imi stream))

  (define (stream-group len strm)
    (and strm
         (lambda ()
           (let loop ([n 0]
                      [ls '()]
                      [strm strm])
             (cond
               [(= n len)
                (values (reverse ls)
                        (stream-group len strm))]
               [strm
                (receive (val rest) (strm)
                  (loop (+ 1 n)
                        (cons val ls)
                        rest))]
               [else
                (error 'stream-group
                       "length of stream is not a multiple of len"
                       len)])))))


  )

