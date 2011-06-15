(library (imi sugar syntax wrapper)
  (export wrap-syntax
          syntax-wrapped-call
          syntax-wrapped-apply)
  (import (rnrs))

  (define (wrap-syntax proc)
    (lambda (context . args)
      (datum->syntax
        context
        (apply proc (map syntax->datum args)))))

  (define (syntax-wrapped-call context proc . args)
    (datum->syntax
      context
      (apply proc (map syntax->datum args))))

  (define (syntax-wrapped-apply context proc args)
    (datum->syntax
      context
      (apply proc (map syntax->datum args))))

  )
