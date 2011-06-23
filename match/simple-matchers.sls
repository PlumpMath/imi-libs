#!r6rs

(library (imi match simple-matchers)
  (export equal-matcher)
  (import (rnrs))

  ;;; matches just by comparing `pattern` and `expr`
  ;;; with `equal?`
  (define (equal-matcher pattern matcher)
    (lambda (expr)
      (and (equal? expr pattern)
           '())))

  )

