#!r6rs

(library (imi asm utils)
  (export case-match
          check)
  (import (rnrs)
          (imi match basic)
          (imi match extend)
          (imi match extensions))

  (define match-always-else
    (make-simple-extension
      'else
      (lambda (pattern matcher)
        (lambda (expr)
          '()))))

  (define asm-matcher
    (matcher-extend basic-matcher*
                    match-symbols
                    label-matches
                    match-alternatives
                    match-and
                    match-procedures-simple
                    match-always-else))

  (define (get/false alist what)
    (cond
      [(assq what alist) => cdr]
      [else #f]))

  (define-syntax case-match
    (lambda (stx)
      (syntax-case stx ()
        [(_ expr rest ...)
         (not (identifier? #'expr))
         #'(let ([x expr])
             (case-match x rest ...))]
        [(_ expr
           [pattern (arg ...) body0 body ...]
           ...)
         (with-syntax ([(matcher ...)
                        (generate-temporaries #'(pattern ...))])
           #'(let ([matcher (asm-matcher `pattern)]
                   ...)
               (cond
                 [(matcher expr)
                  => (lambda (res)
                       (let ([arg (get/false res 'arg)]
                             ...)
                         body0
                         body ...))]
                 ...)))])))


  (define (check what pred who errormsg)
    (unless (pred what)
      (error who errormsg what)))


  )

