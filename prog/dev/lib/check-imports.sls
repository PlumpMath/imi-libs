#!r6rs

(library (imi prog dev lib check-imports)
  (export double-bindings)
  (import (rnrs)
          (imi lib utils)
          (imi proc compose)
          (imi list)
          (imi list processing))

  (define (double-bindings get-lib imports)
    (apply append
           (map/rest (lambda (test-import rest)
                       (remp (compose null? cdr)
                             (map (lambda (check-import)
                                    (cons (map car
                                               (list test-import
                                                     check-import))
                                          (intersect
                                            symbol=?
                                            (cdr test-import)
                                            (cdr check-import))))
                                  rest)))
                     (map (lambda (import)
                            (cons (car import)
                                  (imported-bindings get-lib import)))
                          imports))))


  )

