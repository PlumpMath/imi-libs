#!r6rs

(library (imi list alist)
  (export alist->getq
          alist->getv
          alist->getter)
  (import (rnrs))

  (define (alist->getq alist)
    (lambda (sth)
      (cdr (assq sth alist))))

  (define (alist->getv alist)
    (lambda (sth)
      (cdr (assv sth alist))))

  (define (alist->getter alist)
    (lambda (sth)
      (cdr (assoc sth alist))))

  )

