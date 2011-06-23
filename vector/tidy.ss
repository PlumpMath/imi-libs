#!r6rs

(library (imi vector tidy)
  (export vector-set)
  (import (rnrs)
          (imi sugar for)
          (imi iter iterators))

  (define (vector-set vec n val)
    (let ([newvec (make-vector (vector-length vec))])
      (for loop ([x in (iter-range)]
                 [v in (iter-vector vec)])
           newvec
        (vector-set!
          newvec
          x
          (if (= x n)
              val
              v))
        (loop))))

  )
