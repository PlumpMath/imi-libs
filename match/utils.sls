#!r6rs

(library (imi match utils)
  (export qm-match)
  (import (rnrs))

  (define (qm-match pattern expr)
    (cond
      [(qm? pattern) #t]
      [(pair? pattern)
       (and (pair? expr)
            (qm-match (car pattern)
                      (car expr))
            (qm-match (cdr pattern)
                      (cdr expr)))]
      [else
       (eq? pattern expr)]))

  (define (qm? expr)
    (eq? expr '?))

  )

