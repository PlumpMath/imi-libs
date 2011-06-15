(library (imi utils parameter)
  (export make-parameter)
  (import (rnrs))

  (define make-parameter
    (case-lambda
      ((x) (make-parameter x (lambda (n) n)))
      ((x test)
        (let ([v x])
          (case-lambda
            (() v)
            ((n) (set! v (test n))))))))

  )
