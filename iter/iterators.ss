#!r6rs

(library (imi iter iterators)
  (export iter-list

          vec-like-iterator
          iter-vector
          iter-bytevector-u8
          iter-bytevector-s8
          iter-string

          iter-range
          

          vec-like-subiterator
          iter-subvector
          iter-subbytevector-u8
          iter-subbytevector-s8
          iter-substring

          iter-sublist
          
          iter-input
          do-while
          do-until
          iter-proc)
  (import (rnrs)
          (imi math)
          (imi list)
          (imi proc compose)
          (imi iter iterator)
          (imi sugar for))

  (define (iter-list ls)
    (and (not (null? ls))
         (iterate
           (car ls)
           (iter-list (cdr ls)))))

  (define (vec-like-iterator ref len)
    (lambda (data)
      (let ([data-len (len data)])
        (let loop ([i 0])
          (and (< i data-len)
               (iterate
                 (ref data i)
                 (loop (add1 i))))))))

  (define iter-vector
    (vec-like-iterator vector-ref vector-length))

  (define iter-bytevector-u8
    (vec-like-iterator bytevector-u8-ref bytevector-length))

  (define iter-bytevector-s8
    (vec-like-iterator bytevector-s8-ref bytevector-length))

  (define iter-string
    (vec-like-iterator string-ref string-length))

  (define iter-range
    (case-lambda
      [() (iter-range #f)]
      [(to) (iter-range 0 to)]
      [(from to) (iter-range from 
                             (if (< from to)
                                 1
                                 -1)
                             to)]
      [(from step to)
       (and (or (not to)
                (not (= from to)))
            (iterate
              from
              (iter-range (+ from step)
                          step
                          to)))]))



  (define (vec-like-subiterator ref len)
    (lambda (data . specs)
      (for next ([i in (apply iter-range specs)])
           #f
        (and (< i (len data))
             (iterate
               (ref data i)
               (next))))))

  (define iter-subvector
    (vec-like-subiterator vector-ref vector-length))

  (define iter-subbytevector-u8
    (vec-like-subiterator bytevector-u8-ref bytevector-length))

  (define iter-subbytevector-s8
    (vec-like-subiterator bytevector-s8-ref bytevector-length))

  (define iter-substring
    (vec-like-subiterator string-ref string-length))


  (define iter-sublist
    (case-lambda
      [(ls to) (iter-sublist ls 0 to)]
      [(ls from to) (iter-sublist ls from 1 to)]
      [(ls from step to)
       (let loop ([ls (list-tail ls from)]
                  [pos from])
         (and (< pos to) (not (length< ls step))
              (iterate
                (car ls)
                (loop (list-tail ls step)
                      (+ pos step)))))]))


  (define iter-input
    (case-lambda
      [() (iter-input read)]
      [(reader) (do-until reader eof-object?)]))

  (define (do-while proc go-on?)
    (let loop ()
      (let ([obj (proc)])
        (and (go-on? obj)
             (iterate
               obj
               (loop))))))

  (define (do-until proc end?)
    (do-while proc (neg end?)))

  (define (iter-proc proc)
    (let loop ()
      (iterate
        (proc)
        (loop))))

  )
