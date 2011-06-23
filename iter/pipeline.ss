#!r6rs

(library (imi iter pipeline)
  (export >>=
          pipe-return
          pipe-put
          pipe-get
          <-
          pipeline
          pipe)
  (import (rnrs)
          (imi sugar cut)
          (imi list utils)
          (imi iter iterator))

  (define (>>= action next)
    (lambda (iter)
      (call-with-values
        (cut action iter)
        (lambda (iter . args)
          ((apply next args)
           iter)))))

  (define (pipe-return . vals)
    (lambda (iter)
      (apply values
             iter
             vals)))

  (define (pipe-put val)
    (lambda (iter)
      (iterate val iter)))

  (define (pipe-get)
    (lambda (iter)
      (values (iterator-next iter)
              (iterator-value iter))))

  (define-syntax <-
    (lambda (stx)
      (error '<- "wrong use of auxiliary keyword" stx)))

  (define-syntax pipeline
    (lambda (stx)
      (syntax-case stx (<- =>)
        [(_) #'(pipe-return)]
        [(_ lastexpr)
         #'(pipe-return lastexpr)]

        [(_ before ...
            expr0 => expr1)
         #'(>>= (pipeline before ... expr0)
                expr1)]
        [(_ (vars ...) <- expr
            rest ...)
         #'(>>= expr
                (lambda (vars ...)
                  (pipeline rest ...)))]
        [(_ underscore <- expr
            rest ...)
         (symbol=? (syntax->datum #'underscore)
                   '_)
         #'(>>= expr
                (lambda forget-about-me
                  (pipeline rest ...)))]

        [(_ var <- expr
            rest ...)
         #'(pipeline (var) <- expr
                     rest ...)]
        [(_ expr rest ...)
         #'(pipeline _ <- expr
                     rest ...)])))

  (define (pipe iter pipeline)
    (pipeline iter))

  )
