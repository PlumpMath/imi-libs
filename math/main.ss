(library (imi math)
  (export sub1
          add1
          *pi*)
  (import (rnrs))

  ;;; some helpful small procedures

  (define (sub1 n)
    (- n 1))

  (define (add1 n)
    (+ n 1))

  ;;; mathematical constants

  (define *pi* 3.14159653589793)

  )
