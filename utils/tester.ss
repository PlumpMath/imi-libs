(library (imi utils tester)
  (export tester
          tester*
          testq
          testqv
          test
          char-in)
  (import (rnrs)
          (imi sugar cut))

  (define (tester test eq? ls)
    (lambda (x)
      (test (cut eq? x <>) ls)))

  (define (tester* test eq? . ls)
    (tester test eq? ls))

  (define (testq test . ls)
    (tester test eq? ls))

  (define (testqv test . ls)
    (tester test eqv? ls))

  (define (test proc . ls)
    (tester proc equal? ls))

  (define (char-in chars)
    (tester exists
            char=? 
            (string->list chars)))

  )
