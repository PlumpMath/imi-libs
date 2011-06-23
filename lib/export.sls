#!r6rs

(library (imi lib export)
  (export export-renames)
  (import (rnrs))

  (define (export-renames exports)
    (remp not
          (map export-rename exports)))

  (define (export-rename export)
    (cond
      [(assq 'rename-from (cdr export))
       => (lambda (rename)
            (cons (cdr rename)
                  (car export)))]
      [else #f]))

  )

