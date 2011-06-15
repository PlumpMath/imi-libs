(library (imi foreign c-struct compat)
  (export c-struct-definitions)
  (import (rnrs)
          (imi list processing)
          (imi foreign utils)
          (imi foreign utils compat)
          (imi sugar syntax append)
          (ikarus foreign))

  (define (c-struct-definitions def name slots)
    (with-syntax ([pointer->struct (syntax-append def 'pointer-> name)]
                  [struct->pointer (syntax-append def name '->pointer)]
                  [struct? (syntax-append def name '?)]
                  [make-struct (syntax-append def 'make '- name)])
      (let loop ([specs slots]
                 [offset 0]
                 [definitions #'()]
                 [slot-setters #'()])
        (define (definitions-of slottype slotname rest)
          (with-syntax ([getter-name 
                          (syntax-append def
                            name '- slotname)]
                        [setter-name
                          (syntax-append def
                            name '- slotname '- 'set!)]
                        [offset-syntax (datum->syntax def offset)]
                        [slottype-syntax slottype])
            (loop rest
                  (+ offset
                     (sizeof
                       (syntax->datum slottype)))
                  (cons*
                    #'(define getter-name
                        (let ([getter (get-getter 'get-getter 'slottype-syntax)])
                          (lambda (struct)
                            (getter
                              (struct->pointer struct)
                              offset-syntax))))

                    #'(define setter-name
                        (let ([setter (get-setter 'get-setter 'slottype-syntax)])
                          (lambda (struct val)
                            (setter
                              (struct->pointer struct)
                              offset-syntax
                              val))))

                    definitions)
                  (cons #'setter-name
                        slot-setters))))

        (syntax-case specs ()
          [()
           (with-syntax ([offset-syntax offset]
                         [(slot-setters ...) slot-setters]
                         [(definitions ...) definitions])
                ; little hackish :D
                ; defines pointer->struct, struct? and struct->pointer
             #'((define-record-type (name pointer->struct struct?)
                  (fields (immutable pointer struct->pointer)))

                (define make-struct
                  (lambda vals
                    (let ([p (malloc offset-syntax)])
                      (with-exception-handler
                        (lambda (con)
                          (free p)
                          (raise con))
                        (lambda ()
                          (for-each (lambda (setter val)
                                      (setter p val))
                                    (list slot-setters ...)   ; as setters are in reverse order
                                    (reverse vals))))         ;  because of iterative recursion
                      (pointer->struct p))))

                definitions ...))]

          [((type slotname) rest ...)
           (definitions-of
             #'type
             #'slotname
             #'(rest ...))]

          [((type slotname slotrest ...) rest ...)
           (definitions-of
             #'type
             #'slotname
             #'((type slotrest ...) rest ...))]))))

  )
