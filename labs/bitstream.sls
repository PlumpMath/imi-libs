#!r6rs

(library (imi labs bitstream)
  (export bitstream)
  (import (rnrs)
          (imi sugar rec)
          (imi labs stream))

  (define (bitstream size strm)
    (let loop ([bitpos 0]
               [act #f]
               [strm strm])
      (rec self
        (case-lambda
          [() (self 1)]
          [(n)
           (case n
             [(skip) (loop 0 #f strm)]
             [else
              (cond
                [(not act)
                 (call-with-values strm
                   (lambda (val next)
                     ((loop 0 val next) n)))]
                [(> n (- size bitpos))
                 (call-with-values
                   (lambda () ((loop 0 #f strm)
                               (- n
                                  (- size bitpos))))
                   (lambda (val next)
                     (values (+ (fxgetbits act bitpos size)
                                (fxarithmetic-shift-left
                                  val
                                  (- size bitpos)))
                             next)))]
                [else
                 (values (fxgetbits act bitpos (+ bitpos n))
                         (loop (+ bitpos n)
                               act
                               strm))])])]))))


  (define (fxgetbits num start end)
    (fxand (fxarithmetic-shift-right num start)
           (fx- (fxarithmetic-shift-left 1 (- end start))
                1)))

  )

