#!r6rs

(library (imi sugar static-define)
  (export define/static)
  (import (rnrs))

  (define-syntax define/static
    (lambda (x)
      (syntax-case x ()
        [(_ (proc . params) static-vars body ...)
         #'(define proc
             (let static-vars
               (lambda params
                 body ...)))])))

  )
