#!r6rs

(library (imi list permutation)
  (export list-rotate-right
          list-rotate-left
          list-combinations)
  (import (rnrs)
          (imi sugar list-let)
          (imi list processing))

  (define (list-rotate-right ls amount)
    (list-rotate-left ls (- (length ls)
                            amount)))

  (define (list-rotate-left ls amount)
    (list-let (end start) (list-split-at* ls
                                          (mod amount
                                               (length ls)))
      (append start end)))


  (define (list-combinations ls0 ls1)
    (if (null? ls0)
        '()
        (append (map (lambda (e)
                       (cons (car ls0) e))
                     ls1)
                (list-combinations (cdr ls0) ls1))))

  )

