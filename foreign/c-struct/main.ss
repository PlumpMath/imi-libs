(library (imi foreign c-struct)
  (export define-c-struct
          c-struct-definitions)
  (import (rnrs)
          (imi foreign c-struct compat))

  (define-syntax define-c-struct
    (lambda (stx)
      (syntax-case stx ()
        [(def name slots ...)
         (with-syntax ([(definitions ...)
                        (c-struct-definitions #'def
                                              #'name
                                              #'(slots ...))])
           #'(begin definitions ...))])))

  )
