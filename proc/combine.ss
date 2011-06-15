(library (imi proc combine)
  (export combine
          pipe)
  (import (rnrs)
          (rnrs eval) ; FIXME: unneeded?
          (imi list construct) ; FIXME: unneeded?
          (imi utils gensym)) ; FIXME: unneeded?

  ;;; DRAFTS

  ; already in (imi proc compose)
  (define (combine proc . rest)
    (if (null? rest)
      proc
      (lambda args
        (call-with-values
          (lambda ()
            (apply (apply combine
                          rest)
                   args))
          proc))))

  ; useless?
  (define-syntax pipe
    (lambda (stx)
      (syntax-case stx ()
        [(pipe (args ...) proc)
         #'(proc args ...)]
        [(pipe (args ...) procs ... proc)
         #'(call-with-values
             (lambda () (pipe (args ...) procs ...))
             proc)])))

  )
