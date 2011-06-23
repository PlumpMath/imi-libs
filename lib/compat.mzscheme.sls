#!r6rs

(library (imi lib compat)
  (export library-path
          standard-library?
          standard-library-exports)
  (import (rnrs)
          (prefix (racket) rkt:)
          (setup dirs))

  (define (rkt-list->scm-list ls)
    (if (rkt:null? ls)
        '()
        (cons (rkt:car ls)
              (rkt-list->scm-list
                (rkt:cdr ls)))))

  (define (library-path)
    (map rkt:path->string
         (rkt-list->scm-list
           (get-collects-search-dirs))))

  (define (standard-library? lib)
    #f) ;as I wouldn't know how to get the exported bindings

  (define (standard-library-exports lib)
    (error 'standard-library-exports
           "can't library exports (implementation restriction)"
           lib))
  
  )

