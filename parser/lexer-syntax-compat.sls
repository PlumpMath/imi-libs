#!r6rs

(library (imi parser lexer-syntax-compat)
  (export rule-name
          ignore-name
          simple-name
          rule-pred
          %any?
          %eof?
          %not?
          %error?)
  (import (rnrs))

  (define (symbol-append . ls)
    (string->symbol
      (apply string-append
             (map (lambda (x)
                    (if (symbol? x)
                        (symbol->string x)
                        (symbol->string
                          (syntax->datum x))))
                  ls))))

  (define (rule-name stx)
    (datum->syntax stx (symbol-append '$ stx)))

  (define (ignore-name stx)
    (datum->syntax stx (symbol-append 'ignore- stx)))

  (define (simple-name stx)
    (datum->syntax stx (symbol-append 'simple- stx)))

  (define (rule-pred stx)
    (datum->syntax stx (symbol-append '$ stx '?)))


  (define (symbol-checker symb)
    (lambda (syn)
      (eq? symb (syntax->datum syn))))

  (define %any? (symbol-checker '%any))
  (define %eof? (symbol-checker '%eof))
  (define %not? (symbol-checker '%not))
  (define %error? (symbol-checker '%error))


  )

