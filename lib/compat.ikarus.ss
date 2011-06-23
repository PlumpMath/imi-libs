#!r6rs

(library (imi lib compat)
  (export library-path
          standard-library?
          standard-library-exports)
  (import (rnrs)
          (ikarus))

  (define (standard-library? lib)
    (case (car lib)
      [(rnrs ikarus) #t]
      [else #f]))

  (define (standard-library-exports lib)
    (environment-symbols
      (environment lib)))
  
  )
