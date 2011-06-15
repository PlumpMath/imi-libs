(library (imi foreign import-c-library)
  (export import-c-library)
  (import (rnrs)
          (imi foreign c-library)
          (imi foreign c-array)
          (imi foreign c-struct)
          (imi foreign c-procedure)
          (imi foreign c-loader)
          (imi foreign utils)
          (imi sugar syntax wrapper))


  (define-syntax import-c-library
    (lambda (stx)
      (define (convert-defs constructs)
        (if (null? constructs)
          '()
          (syntax-case (car constructs) (struct *)
            [(struct name specs ...)
             (append
               (c-struct-definitions #'struct #'name #'(specs ...))
               (convert-defs (cdr constructs)))]

            [(type * c-name)
             (identifier? #'c-name)
             (with-syntax ([scm-name 
                             (syntax-wrapped-call
                               #'imp
                               c-symbol->scm-symbol
                               #'c-name)])
               (convert-defs
                 (cons
                   #'(type * (c-name scm-name))
                   (cdr constructs))))]
            [(type * (c-name scm-name))
             (cons
               #'(define scm-name
                  (make-c-array
                    'type
                    (dlsym clibrary (symbol->string 'c-name))))
               (cdr constructs))]

            [(type c-name (args ...))
             (identifier? #'c-name)
             (with-syntax ([scm-name
                             (syntax-wrapped-call
                               #'imp
                               c-symbol->scm-symbol
                               #'c-name)])
               (convert-defs
                 (cons
                   #'(type (c-name scm-name) (args ...))
                   (cdr constructs))))]
            [(type (c-name scm-name) (args ...))
             (cons
               #'(define scm-name
                   ((make-c-callout 'type '(args ...))
                    (dlsym clibrary (symbol->string 'c-name))))
               (convert-defs (cdr constructs)))])))

      (syntax-case stx ()
        [(imp path
              defs ...)
         (with-syntax ([(definitions ...) (convert-defs #'(defs ...))])
           #'(begin
               (define clibrary
                 (or (open-c-library 'path)
                     (error 'import-c-library "could not open library" 'path)))
               definitions ...))])))

  )
