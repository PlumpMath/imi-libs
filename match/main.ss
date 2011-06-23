#!r6rs

(library (imi match)
  (export simple-match
          match-equal?)
  (import (rnrs))

  (define (match-equal? a b)
    (equal? a b))

  (define (placeholder? x)
    (and (symbol? x)
         (symbol=? x '_)))

  (define (ellipsis? x)
    (and (symbol? x)
         (symbol=? x '...)))

  (define (constant? x)
    (and (list? x)
         (length=? x 2)
         (symbol? (car x))
         (symbol=? (car x) 'quote)))

  (define (constant-expr x)
    (cadr x))

  (define (simple-match pattern in)
    (cond
      [(constant? pattern)
       (match-equal? (constant-expr pattern)
                     in)]
      [(symbol? pattern)
       (cons pattern in)]
      [(list? pattern)
       (and (list? in)
            (simple-match-list pattern in))]
      [(vector? pattern)
       (and (vector? in)
            (simple-match-list
              (vector->list pattern)
              (vector->list in)))]
      [else
       (match-equal? pattern in)]))

  (define (simple-match-list pattern expr)
    (let loop ([pattern (reverse pattern)]
               [expr (reverse expr)]
               [vars '()])
      
