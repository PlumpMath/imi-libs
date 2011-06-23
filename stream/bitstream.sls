#!r6rs

(library (imi stream bitstream)
  (export bitstream
          number->bitstream
          bitstream-block
          bitstream-blockify)
  (import (rnrs)
          (imi sugar receive)
          (imi stream))

  (define (bitstream endianness input-width input)
    (stream-flatten1
      (stream-map (lambda (num)
                    (number->bitstream endianness
                                       input-width
                                       num))
                  input)))


  (define (number->bitstream endianness width num)
    (let loop ([pos 0])
      (and (< pos width)
           (lambda ()
             (values (get-bit endianness num width pos)
                     (loop (+ 1 pos)))))))


  (define (get-bit endianness num width bit)
    (fxand (bitwise-arithmetic-shift-right
             num
             (case endianness
               [(little) bit]
               [(big) (- width bit)]))
           1))



  (define (bitstream-block endianness width strm)
    (receive (bits rest) (stream-take/drop width strm)
      (values (bitlist->number endianness bits)
              rest)))

  (define (bitlist->number endianness bitlist)
    (let loop ([bits (case endianness   ;most significant bit first
                       [(little) (reverse bitlist)]
                       [(big) bitlist])]
               [num 0])
      (cond
        [(null? bits) num]
        [else
         (loop (cdr bits)
               (+ (car bits)
                  (bitwise-arithmetic-shift-left num 1)))])))


  (define (bitstream-blockify endianness width strm)
    (stream-mapsequence (lambda (stream)
                          (bitstream-block endianness
                                           width
                                           stream))
                        strm))



  )

