#!r6rs

(library (imi sugar receive)
  (export receive)
  (import (rnrs))

  (define-syntax receive
    (syntax-rules ()
      [(_ vars expr
          body ...)
       (call-with-values
         (lambda () expr)
         (lambda vars
           body ...))]))

  )

