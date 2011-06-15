(library (imi contract standard)
  (export or/c
          and/c
          cons/c
          list/c
          list*/c
          listof/c
          )
  (import (rnrs)
          (imi contract base))

  (define-syntax or/c
    (syntax-contract-case obj
      [(contract ...) (or (contract obj) ...)]))

  (define-syntax and/c
    (syntax-contract-case obj
      [(contract ...) (and (contract obj) ...)]))

  (define-syntax cons/c
    (syntax-contract-case obj 
      [(car/c cdr/c)
        (and (pair? obj)
             (car/c (car obj))
             (cdr/c (cdr obj)))]))

  (define-syntax list/c
    (syntax-contract-case obj
      [() => null?]
      [(arg rest ...)
        => (cons/c arg (list/c rest ...))]))

  (define-syntax list*/c
    (syntax-contract-case obj
      [(arg) => arg]
      [(arg rest ...)
       => (cons/c arg (list*/c rest ...))]))

  (define-syntax listof/c
    (syntax-contract-case obj
      [(arg) => (or/c null? (cons/c arg (listof/c arg)))]))

  )
