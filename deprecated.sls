#!r6rs

(library (imi deprecated)
  (export check-deprecation
          mark-deprecated)
  (import (rnrs)
          (imi lib))

  (define check-deprecation
    (case-lambda
      [(lib)
       (if (equal? lib '(imi deprecated))
           'deprecated
           (call-with-values
             (lambda () (fetch-library lib))
             (lambda (name exports imports body)
               (let ([deprecated-imports
                       (remp not
                             (map check-deprecation
                                  (map car imports)))])
                 (and (not (null? deprecated-imports))
                      (cons name deprecated-imports))))))]
      [libs (map check-deprecation libs)]))

  (define (display/nl str)
    (display str)
    (newline))

  (define (mark-deprecated)
    (for-each display/nl
      '("Warning: You may have imported a deprecated library"
        "         please check with"
        "           (check-deprecation '(your library name))"
        "         or"
        "           (check-deprecation '(your)"
        "                              '(application)"
        "                              '(imports))"
        "         from (imi deprecated). There you can see if"
        "         you imported a deprecated library by yourself"
        "         or if any of your used libraries import"
        "         deprecated libraries."
        ""
        "         Deprecated libraries are marked by importing"
        "         this library (imi deprecated) and calling"
        "           (mark-deprecated)"
        "         .")))

  )

