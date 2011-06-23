#!r6rs

(library (imi match lambda)
  (export match-lambda)
  (import (rnrs))

  (define (get sth alist)
    (let ([x (assq sth alist)])
      (and x (cdr x))))

  (define-syntax match-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ matcher
            [pattern (var ...) body0 body ...]
            ...)
         (with-syntax ([(match-proc ...)
                        (generate-temporaries #'(pattern ...))])
           #'(let ([m matcher])
               (let ([match-proc (m `pattern m)] ...)
                 (lambda args
                   (cond
                     [(match-proc args)
                      => (lambda (res)
                           (let ([var (get 'var res)] ...)
                             body0 body ...))]
                     ...
                     [else (error 'match-lambda
                                  "invalid args - no match"
                                  args)])))))])))


  )

