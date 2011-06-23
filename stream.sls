#!r6rs

(library (imi stream)
  (export stream-cons
          stream->pair
          stream->list
          stream-length$

          stream-append
          stream-flatten1
          stream-take
          stream-take/drop

          stream-map
          stream-mapsequence
          stream-fold-left)
  (import (rnrs)
          (imi sugar receive))

  (define (stream-cons val rest)
    (lambda ()
      (values val rest)))



  (define (stream->pair strm)
    (call-with-values strm cons))



  (define (stream->list strm)
    (let loop ([strm strm])
      (if strm
          (receive (val rest) (strm)
            (cons val (loop rest)))
          '())))

  (define (stream-length$ strm)
    (let loop ([len 0]
               [strm strm])
      (if strm
          (receive (val rest) (strm)
            (loop (+ 1 len)
                  rest))
          len)))




  (define (stream-append strm . streams)
    (if (null? streams)
        strm
        (if strm
            (lambda ()
              (receive (val rest) (strm)
                (values val
                        (apply stream-append rest streams))))
            (apply stream-append streams))))


  (define (stream-flatten1 strm)
    (and strm
         (receive (actstrm rest) (strm)
           (stream-append actstrm
                          (stream-flatten1 rest)))))


  (define (stream-take n strm)
    (if (zero? n)
        '()
        (receive (val rest) (strm)
          (cons val
                (stream-take (- n 1)
                             rest)))))

  
  (define (stream-take/drop n stream)
    (let loop ([ls '()]
               [n n]
               [strm stream])
      (cond
        [(zero? n)
         (values (reverse ls)
                 strm)]
        [(not strm)
         (error 'stream-take/drop
                "stream is too short"
                n stream)]
        [else
         (receive (val rest) (strm)
           (loop (cons val ls)
                 (- n 1)
                 rest))])))




  (define (stream-map proc . streams)
    (if (exists not streams)
        #f
        (lambda ()
          (let ([pairs (map stream->pair streams)])
            (values (apply proc (map car pairs))
                    (apply stream-map proc (map cdr pairs)))))))



  (define (stream-mapsequence proc strm)
    (and strm
         (lambda ()
           (receive (val rest) (proc strm)
             (values val
                     (stream-mapsequence proc rest))))))


  (define (stream-fold-left proc init strm)
    (if strm
        (receive (val rest) (strm)
          (stream-fold-left proc
                            (proc init val)
                            rest))
        init))


  )

