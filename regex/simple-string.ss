(library (imi regex simple-string)
  (export regex-simple-string?
          regex-simple-string)
  (import (rnrs)
          (imi math))

  (define (regex-simple-string? rexpr)
    (string? rexpr))

  (define (regex-simple-string rexpr)
    (let ([search (string->list rexpr)])
      (lambda (str pos content matches)
        (let loop ([str str]
                   [pos pos]
                   [search search]
                   [content content])
          (cond
            [(null? search)
              (matches str pos content #f)]
            [(null? str) #f]
            [(char=? (car str) (car search))
             (loop (cdr str) (add1 pos)
                   (cdr search)
                   (cons (car str) content))]
            [else #f])))))

  )
