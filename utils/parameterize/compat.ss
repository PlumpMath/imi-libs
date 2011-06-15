(library (imi utils parameterize compat)
  (export parameterize
          parameterize-bind
          bind-parameters-to-values
          parameter-set-value)
  (import (rnrs))

  (define-syntax parameterize
    (lambda (x)
      (syntax-case x ()
        ((parameterize ((parameter value) ...) expr ...)
          #'(parameterized-bind (list parameter ...)
                                (list value ...)
                                (lambda () expr ...))))))

  (define (parameterized-bind parameters new-values body)
    (let ((old-values (map parameter-get-value parameters)))
      (dynamic-wind
        (lambda ()
          (bind-parameters-to-values parameters new-values))
        body
        (lambda ()
          (bind-parameters-to-values parameters old-values)))))
    
  (define (bind-parameters-to-values parameters values)
    (for-each parameter-set-value parameters values))

  (define (parameter-get-value parameter)
    (parameter))

  (define (parameter-set-value parameter value)
    (parameter value))

  )
