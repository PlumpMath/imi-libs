#!r6rs

(library (imi sugar list-let)
  (export list-let)
  (import (rnrs))

  (define-syntax list-let
    (syntax-rules ()
      [(list-let vars expr
         body0 body ...)
       (apply (lambda vars
                body0 body ...)
              (let ([x expr])
                (if (list? x)
                    x
                    (error 'list-let
                           "not a list"
                           x))))]))

  )

