#!r6rs

(library (imi hashing crc)
  (export crc32)
  (import (rnrs))

  (define *crc32-poly* #x04C11DB7)

  (define (largest-bit32 num)
    (bitwise-bit-field num 31 32))

  (define shift-left bitwise-arithmetic-shift-left)
  (define xor bitwise-xor)

  (define (crc32 bitstream)
    (let loop ([crc32 0]
               [strm bitstream])
      (if strm
          (call-with-values strm
            (lambda (bit strm)
              (loop (if (= bit (largest-bit32 crc32))
                        (shift-left crc32 1)
                        (xor (shift-left crc32 1)
                             *crc32-poly*))
                    strm)))
          crc32)))


  )

