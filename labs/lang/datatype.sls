#!r6rs

(library (imi labs lang datatype)
  (export define-datatype
          cases)
  (import (rnrs))

  (define-syntax define-datatype
    (syntax-rules ()
      [(_ datapred
          [sub (slot pred) ...]
          ...)
       (begin (define (datapred x)
                (and (pair? x)
                     (or (eq? sub (car x))
                         ...)))
              (define (sub slot ...)
                (if (and (pred slot) ...)
                    (list sub slot ...)
                    (error 'sub "invalid arguments" slot ...)))
              ...)]))

  (define-syntax cases
    (syntax-rules ()
      [(_ x [sub (slot ...) body ...] ...)
       (let ([tmp x])
         (cond
           [(not (pair? tmp))
            (error 'cases "not a datatype" tmp)]
           [(eq? sub (car tmp))
            (apply (lambda (slot ...)
                     body ...)
                   (cdr tmp))]
           ...))]))

  )

