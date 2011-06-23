#!r6rs

(library (imi iter processing)
  (export iter-map)
  (import (rnrs)
          (imi iter iterator))

  (define (iter-map proc . iterators)
    (iterate (apply proc
                    (map iterator-value iterators))
             (apply iter-map
                    proc
                    (map iterator-next iterators))))

  )
