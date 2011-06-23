#!r6rs

(library (imi list match)
  (export simple-match
          call/simple-match)
  (import (rnrs)
          (imi sugar and-let))

  (define (simple-match keywords pattern obj)
    (cond
      [(pair? pattern)
       (and-let [(pair? obj)]
                [match-car (simple-match keywords
                                         (car pattern)
                                         (car obj))]
                [match-cdr (simple-match keywords
                                         (cdr pattern)
                                         (cdr obj))]
                (append match-car match-cdr))]
      [(symbol? pattern)
       (if (memq pattern keywords)
           (if (eq? pattern obj)
               '()
               #f)
           (list (cons pattern obj)))]
      [(null? pattern)
       (and (null? obj)
            '())]
      [else
       (and (eq? pattern obj)
            '())]))

  (define (call/simple-match keywords pattern obj proc)
    (let ([match (simple-match keywords pattern obj)])
      (and match
           (apply proc (map cdr match)))))

  )
