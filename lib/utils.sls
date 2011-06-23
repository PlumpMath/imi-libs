#!r6rs

(library (imi lib utils)
  (export imported-bindings)
  (import (rnrs)
          (imi sugar cut)
          (imi proc dispatcher)
          (imi lib parser))

  (define (imported-bindings get-lib import)
    (fold-left (lambda (bindings proc)
                 (proc bindings))
               (get-lib-exports get-lib (car import))
               (map import-spec->proc (cdr import))))

  (define (get-lib-exports get-lib lib)
    (call-with-values
      (cut get-lib lib)
      (lambda (name exports imports body)
        (map car exports))))

  (define (import-spec->proc import-spec)
    (dispatchq (car import-spec)
               *import-processors*
               (cdr import-spec)))

  (define (rename-proc rename-spec)
    (let ([from (car rename-spec)]
          [to (cadr rename-spec)])
      (lambda (bindings)
        (map (lambda (binding)
               (if (symbol=? from binding)
                   to
                   binding))
             bindings))))

  (define (prefix-proc prefix-spec)
    (let ([prefix prefix-spec])
      (lambda (bindings)
        (map (cut symbol-append prefix <>)
             bindings))))

  (define (except-proc except-spec)
    (lambda (bindings)
      (remp (cut memq <> except-spec)
            bindings)))

  (define (only-proc only-spec)
    (lambda (bindings)
      (filter (cut memq <> only-spec)
              bindings)))

  (define *import-processors*
    `((rename . ,rename-proc)
      (prefix . ,prefix-proc)
      (except . ,except-proc)
      (only   . ,only-proc)
      ))

  (define (symbol-append symb0 symb1)
    (string->symbol
      (string-append
        (symbol->string symb0)
        (symbol->string symb1))))

  )

