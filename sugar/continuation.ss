(library (imi sugar continuation)
  (export let/cc
          values->list)
  (import (rnrs))

  ;;; binds the current continuation to
  ;;;   a given identifier
  ;;;
  ;;; (let/cc id
  ;;;   expr ...)
  ;;;
  ;;; id : identifier?
  (define-syntax let/cc
    (syntax-rules ()
      [(let/cc var expr ...)
       (call/cc
         (lambda (var)
           expr ...))]))

  ;;; transforms the values returned by
  ;;;   an expression into a list
  ;;;
  ;;; (values->list expr)
  (define-syntax values->list
    (syntax-rules ()
      [(values->list expr)
       (call-with-values
         (lambda () expr)
         (lambda val val))]))


  )
