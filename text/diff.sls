#!r6rs

(library (imi text diff)
  (export diff)
  (import (rnrs)
          (imi stream bytevector-convert)
          (imi hashing md5)
          (imi text processing))

  (define (diff orig changed)
    (let ([orig (hash-lines orig)]
          [changed (hash-lines changed)])
      ))

  (define (hash-lines str)
    (map (lambda (line)
           (cons (md5-string line)
                 line))
         (split-lines str)))

  (define (hashed-line=? ln0 ln1)
    (and (= (car ln0)
            (car ln1))
         (string=? (cdr ln0)
                   (cdr ln1))))

  (define (md5-string str)
    (md5 (string->utf8-bytestream str)))

  )

