(library (imi sugar cut.compat)
  (export transform)
  (import (rnrs))


  (define (transform ls)
    (let loop ([vars '()]
               [call '()]
               [ls ls])
      (if (null? ls)
        (values vars (reverse call))
        (syntax-case (car ls) (<> cut)
          [<>
           (with-syntax ([(new-id) (generate-temporaries '(1))])
             (loop (cons #'new-id vars)
                   (cons #'new-id call)
                   (cdr ls)))]
          [(cut rest ...)
           (loop vars
                 (cons #'(cut rest ...)
                       call)
                 (cdr ls))]
          [(sub ...)
           (let-values ([(subvars subcall)
                         (transform #'(sub ...))])
             (loop (append subvars vars)
                   (cons subcall call)
                   (cdr ls)))]
          [id
           (loop vars
                 (cons #'id call)
                 (cdr ls))]))))

  )
