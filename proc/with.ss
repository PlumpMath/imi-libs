#!r6rs

(library (imi proc with)
  (export with-procedure)
  (import (rnrs)
          (imi utils parameterize))

  ;;; creates a procedure which sets a parameter
  ;;;   before a thunk call and resets it to its
  ;;;   initial value afterwards; the procedure
  ;;;   will behave like with-input-from-file or
  ;;;   similar
  ;;;
  ;;; parameter - (parameter/c type)
  ;;;  -> (-> ret type (-> ret))
  (define (with-procedure parameter)
    (lambda (val thunk)
      (parameterize ([parameter val])
        (thunk))))
  
  )
