(library (imi contract base)
  (export syntax-contract-case)
  (import (rnrs))

  ;;; creates a syntax transformer
  ;;;  to create a contract for a
  ;;;  data type, where the types
  ;;;  for the slots of the data are
  ;;;  given; transforms into a
  ;;;  lambda expression, which is
  ;;;  a predicate with the body
  ;;;  `body`
  ;;;
  ;;; (syntax-contract call-arg transform-args body)
  ;;;
  ;;;  call-arg - identifier?    ; the argument name of
  ;;;                            ;  the predicate procedure
  ;;;  transform-args   ; the arguments for the transformer,
  ;;;                   ;  like in syntax-case, but without
  ;;;                   ;  the syntax-transformer name itself
  ;;;  body    ; the body expression
  ;;;   -> (-> any? any?)
  (define-syntax syntax-contract-case
    (lambda (stx)
      (syntax-case stx ()
        [(_ call-arg
           [transform-args rest ...]
           ...)
         (identifier? #'call-arg)
         (with-syntax ([(scases ...) (map (lambda (ccase)
                                            (syntax-case ccase (=>)
                                              [(transform-args => body)
                                               #'(lambda (call-arg) body)]
                                              [(transform-args body ...)
                                               #'(lambda (call-arg) body ...)]))
                                          #'([transform-args rest ...] ...))])
           (display #'([transform-args #'scases] ...))
           #'(lambda (stx)
               (syntax-case stx ()
                 [(_ . transform-args)
                  #'scases]
                 ...)))])))

  )
