#!r6rs

(library (imi sugar thunk)
  (export thunk)
  (import (rnrs))

  (define-syntax thunk
    (lambda (stx)
      (syntax-case stx ()
        [(_ body ...)
         #'(lambda () body ...)])))

  )
