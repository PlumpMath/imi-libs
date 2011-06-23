#!r6rs

(library (imi labs state-loop)
  (export loop)
  (import (rnrs)
          (imi utils sleep))

  (define (loop sth)
    (when sth
      (let ([d (sth)])
        (display (car d))
        (newline)
        (loop (cdr d)))))

  )

