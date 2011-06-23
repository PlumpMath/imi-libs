#!r6rs

(library (imi hashing md5)
  (export md5
          md5hash->string
          string->md5hash)
  (import (rnrs)
          (imi sugar list-let)
          (imi sugar receive)
          (imi list permutation)
          (imi string utils)
          (imi stream)
          (imi stream utils)
          (imi stream bytestream)
          (prefix (imi math bitwise bit-field)
                  imi:))

  (define (list-range n)
    (let loop ([n n]
               [ls '()])
      (if (zero? n)
          ls
          (loop (- n 1)
                (cons n ls)))))

  (define *sin-table*
    (list->vector
      (map (lambda (x)
             (exact (floor (* (abs (sin x))
                              (expt 2 32)))))
           (list-range 64))))



  (define (md5op abcd ksi proc block)
    (list-let (a b c d) abcd
      (list-let (k s i) ksi
        (list-rotate-right
          (list (word
                  (+ b (rotate-left32
                         (+ a
                            (proc b c d)
                            (vector-ref block k)
                            (vector-ref *sin-table*
                                        (- i 1)))
                         s)))
                b
                c
                d)
          1))))



  (define (md5round md5proc ksi-list)
    (lambda (block abcd)
      (fold-left (lambda (abcd ksi)
                   (md5op abcd ksi md5proc block))
                 abcd
                 ksi-list)))



  ; (((x) & (y)) | ((~x) & (z)))
  (define (md5F x y z)
    (word (lior (land x y)
                (land (lnot x)
                      z))))

  (define md5round1
    (md5round md5F
              '((00 07 01) (01 12 02) (02 17 03) (03 22 04)
                (04 07 05) (05 12 06) (06 17 07) (07 22 08)
                (08 07 09) (09 12 10) (10 17 11) (11 22 12)
                (12 07 13) (13 12 14) (14 17 15) (15 22 16))))


  ; (((x) & (z)) | ((y) & (~z)))
  (define (md5G x y z)
    (word (lior (land x z)
                (land y (lnot z)))))

  (define md5round2
    (md5round md5G
              '((01 05 17) (06 09 18) (11 14 19) (00 20 20)
                (05 05 21) (10 09 22) (15 14 23) (04 20 24)
                (09 05 25) (14 09 26) (03 14 27) (08 20 28)
                (13 05 29) (02 09 30) (07 14 31) (12 20 32))))


  ; ((x) ^ (y) ^ (z))
  (define (md5H x y z)
    (word (lxor x y z)))

  (define md5round3
    (md5round md5H
              '((05 04 33) (08 11 34) (11 16 35) (14 23 36)
                (01 04 37) (04 11 38) (07 16 39) (10 23 40)
                (13 04 41) (00 11 42) (03 16 43) (06 23 44)
                (09 04 45) (12 11 46) (15 16 47) (02 23 48))))


  ; ((y) ^ ((x) | (~z)))
  (define (md5I x y z)
    (word (lxor y
                (lior x
                      (lnot z)))))

  (define md5round4
    (md5round md5I
              '((00 06 49)  (07 10 50)  (14 15 51)  (05 21 52)
                (12 06 53)  (03 10 54)  (10 15 55)  (01 21 56)
                (08 06 57)  (15 10 58)  (06 15 59)  (13 21 60)
                (04 06 61)  (11 10 62)  (02 15 63)  (09 21 64))))



  (define (stream-extend/length strm)
    (define (extend len)
      (lambda ()
        (values #x80 ;(shift-left 1 7)
                (padding/len (+ 1 len)
                             len))))

    (define (padding/len pos len)
      (if (= 56 (mod pos 64))
          (number->bytestream (endianness little)
                              8
                              (* len 8))
          (lambda ()
            (values 0
                    (padding/len (+ 1 pos)
                                 len)))))

    (let loop ([len 0]
               [strm strm])
      (cond
        [(not strm)
         (extend len)]
        [else
         (lambda ()
           (receive (byte rest) (strm)
             (values byte
                     (loop (+ 1 len)
                           rest))))])))


  (define (md5block block abcd)
    (let ([block (list->vector block)])
      (fold-left (lambda (abcd proc)
                   (proc block abcd))
                 abcd
                 (list md5round1
                       md5round2
                       md5round3
                       md5round4))))



  (define (md5 bytestrm)
    (apply number-append32
           (stream-fold-left
             (lambda (abcd block)
               (map word
                    (map +
                         (md5block block abcd)
                         abcd)))
             (list #x67452301
                   #xefcdab89
                   #x98badcfe
                   #x10325476)
             (stream-group
               16
               (bytestream-blockify
                 (endianness little)
                 4
                 (stream-extend/length bytestrm))))))


  #;
  (define (number-append32 a b c d)
    (imi:bitwise-copy-bit-field*
      000
        0   8 a
        8  16 (shift-right a  8)
       16  24 (shift-right a 16)
       24  32 (shift-right a 24)
       32  40 b
       40  48 (shift-right b  8)
       48  56 (shift-right b 16)
       56  64 (shift-right b 24)
       64  72 c
       72  80 (shift-right c  8)
       80  88 (shift-right c 16)
       88  96 (shift-right c 24)
       96 104 d
      104 112 (shift-right d  8)
      112 120 (shift-right d 16)
      120 128 (shift-right d 24)))


  (define (number-append32 a b c d)
    (imi:bitwise-copy-bit-field*
      0
       0  32 a
      32  64 b
      64  96 c
      96 128 d))






  (define (rotate-left32 num rot)
    (imi:bitwise-rotate-bit-field num 0 32 rot))

  (define shift-left bitwise-arithmetic-shift-left)
  (define shift-right bitwise-arithmetic-shift-right)
  (define lior bitwise-ior)
  (define lxor bitwise-xor)
  (define land bitwise-and)
  (define lnot bitwise-not)

  (define (word num)
    (imi:bitwise-bit-field num 0 32))




  (define (byte-pos index)
    (* index 8))

  (define (byte->hex b)
    (string-pad 2 #\0 (number->string b 16)))

  (define (md5hash->string hash)
    (fold-left string-append
               ""
               (map (lambda (pos)
                      (byte->hex
                        (imi:bitwise-bit-field hash
                                               (byte-pos pos)
                                               (byte-pos (+ 1 pos)))))
                    '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))

  (define (string->md5hash str)
    (string->number
      (fold-left string-append
                 ""
                 (reverse (string-group 2 str)))
      16))

  )

