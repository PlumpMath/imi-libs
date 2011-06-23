#!r6rs

(library (imi sugar begin0)
  (export begin0)
  (import (rnrs))

  ;;; evaluates multiple expressions but 
  ;;;   returns value of first expression;
  ;;;   same syntax as begin
  ;;;
  ;;; (begin0 expr ...)
  (define-syntax begin0
    (lambda (x)
      (syntax-case x ()
        [(begin0 expr rest ...)
         #'(let ([t expr])
             (begin rest ...
                    t))])))

  )
