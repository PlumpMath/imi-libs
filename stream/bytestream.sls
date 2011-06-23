#!r6rs

(library (imi stream bytestream)
  (export bytestream
          number->bytestream
          bytestream-block
          bytestream-blockify)
  (import (rnrs)
          (imi sugar receive)
          (imi stream))

  (define (bytestream endianness input-width input)
    (stream-flatten1
      (stream-map (lambda (num)
                    (number->bytestream endianness
                                       input-width
                                       num))
                  input)))


  (define (number->bytestream endianness width num)
    (let loop ([pos 0])
      (and (< pos width)
           (lambda ()
             (values (get-byte endianness num width pos)
                     (loop (+ 1 pos)))))))


  (define (get-byte endianness num width bit)
    (bitwise-and (bitwise-arithmetic-shift-right
                   num
                   (* 8 (case endianness
                          [(little) bit]
                          [(big) (- width bit)])))
                 (- (shift-left 1 8)
                    1)))



  (define (bytestream-block endianness width strm)
    (receive (bytes rest) (stream-take/drop width strm)
      (values (bytelist->number endianness bytes)
              rest)))

  (define (bytelist->number endianness bytelist)
    (let loop ([bytes (case endianness   ;most significant byte first
                       [(little) (reverse bytelist)]
                       [(big) bytelist])]
               [num 0])
      (cond
        [(null? bytes) num]
        [else
         (loop (cdr bytes)
               (+ (car bytes)
                  (shift-left num 8)))])))


  (define (bytestream-blockify endianness width strm)
    (stream-mapsequence (lambda (stream)
                          (bytestream-block endianness
                                            width
                                            stream))
                        strm))

  (define shift-left bitwise-arithmetic-shift-left)



  )

