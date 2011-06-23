#!r6rs

(library (imi help)
  (export find-identifier
          find-identifier/imi
          exports-of
          definitions-of)
  (import (rnrs)
          (imi lib)
          (imi lib parser)
          (imi lib export)
          (imi io file)
          (imi io filesystem)
          (imi sugar cut)
          (imi sugar and-let)
          (imi sugar continuation)
          (imi list utils)
          (imi list processing)
          (imi string processing))

  (define (find-identifier id)
    (let ([id-finder (identifier-in-file-finder id)])
      (apply append
             (map (cut directory-filterrec #f id-finder <>)
                  (filter path-exists?
                          (library-path))))))

  (define (find-identifier/imi id)
    (directory-filterrec
      #f
      (identifier-in-file-finder id)
      (find-in-library-path (cut <> "imi/"))))

  (define (identifier-in-file-finder id)
    (lambda (file)
      (and (not (string-contains? file "compat"))
           (filename->lib file)
           (find-identifier-in-file id file))))

  (define (find-identifier-in-file id file)
    (let/cc escape
      (with-exception-handler
        (lambda (exception) ;if the file is not a well-formed library
          (escape #f))
        (lambda ()
          (call-with-values
            (cut parse-lib (with-input-from-file file read))
            (lambda (name exports imports body)
              (let ([found-ids
                      (map list->symbol
                           (filter (cut list-match-partial
                                        char=?
                                        (symbol->list id)
                                        <>)
                                   (map symbol->list
                                        (map car exports))))])
                (and (not (null? found-ids))
                     (cons name found-ids)))))))))

  (define (symbol->list symb)
    (string->list
      (symbol->string
        symb)))

  (define (list->symbol ls)
    (string->symbol
      (list->string
        ls)))
            

  (define (read-library lib)
    (with-input-from-file
      (or (find-library lib)
          (error 'read-library
                 "could not find library"
                 lib))
      read))



  (define (exports-of lib)
    (call-with-values
      (cut parse-lib (read-library lib))
      (lambda (name exports imports body)
        (map car exports))))


  (define (definitions-of lib)
    (call-with-values
      (cut parse-lib (read-library lib))
      (lambda (name exports imports body)
        (let ([renames (export-renames exports)])
          (map cut-definition
               (map (definition-renamer renames)
                    (filter (definition-filter
                              (map car exports))
                            body)))))))



  (define (definition-filter exports)
    (lambda (expr)
      (and (definition? expr)
           (memq (definition-name expr)
                 exports))))

  (define (definition? sth)
    (and (list? sth)
         (length>? sth 2)
         (memq (car sth) *definers*)))

  (define (definition-name expr)
    (if (list? (cadr expr))
        (caadr expr)
        (cadr expr)))

  (define *definers* '(define define-syntax))



  (define (definition-renamer renames)
    (lambda (def)
      (cons* (car def)
             (if (list? (cadr def))
                 (cons (rename (caadr def)
                               renames)
                       (cdadr def))
                 (rename (cadr def)
                         renames))
             (cddr def))))

  (define (rename name renames)
    (cond
      [(assq name renames) => cdr]
      [else name]))


  
  (define (cut-definition def)
    (if (list? (cadr def))
        (list (car def)
              (cadr def)
              '...)
        def))


  )

