#!r6rs

(library (imi stream convert)
  (export list->stream)
  (import (rnrs))

  (define (list->stream ls)
    (and (not (null? ls))
         (lambda ()
           (values (car ls)
                   (list->stream (cdr ls))))))

  )

