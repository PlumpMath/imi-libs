#!r6rs

(library (imi sugar case-match)
  (export case-match
          case-match*)
  (import (rnrs)
          (imi match basic))


  (define-syntax case-match*
    (syntax-rules (else)
      [(case-match* matcher what
         [else body0 body ...])
       (begin body0 body ...)]
      [(case-match* matcher what
         [pattern vars body0 body ...]
         rest0 rest ...)
       (let ([what-evaluated what]
             [match (matcher 'pattern)])
         (cond
           [(match what-evaluated)
            => (lambda (m)
                 (apply (lambda vars
                          body0 body ...)
                        (map cdr m)))]
           [else
            (case-match* matcher what-evaluated
              rest0 rest ...)]))]))


  (define-syntax case-match
    (syntax-rules ()
      [(case-match what
         case ...)
       (case-match* basic-matcher what
         case ...)]))

  )

