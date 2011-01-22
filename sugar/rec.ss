(library (imi sugar rec)
  (export rec)
  (import (rnrs))

  ;;; assigns an expression to a temporary variable
  ;;;   accessible in the expression and returns
  ;;;   the return value of the expression
  ;;;
  ;;; syntax like define
  (define-syntax rec
    (syntax-rules ()
      [(rec (proc . args) body ...)
       (letrec ([proc (lambda args body ...)]) proc)]
      [(rec name expr)
       (letrec ([name expr]) name)]))

  )
