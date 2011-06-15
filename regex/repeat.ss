(library (imi regex repeat)
  (export regex-repeat?
          regex-repeat)
  (import (rnrs)
          (only (ikarus) add1)
          (imi regex))

  (define (regex-repeat? rexpr)
    (and (list? rexpr) (eq? '** (car rexpr))
         (regex-repeat-spec? (cadr rexpr))
         (regex-repeat-spec? (caddr rexpr))))

  (define (regex-repeat-spec? expr)
    (or (and (integer? expr) (> expr 0))
        (eq? expr '*)))

  (define (regex-repeat rexpr)
    (let ([from (cadr rexpr)]
          [to   (caddr rexpr)]
          [subregexproc (regex (cadddr rexpr))])

      (define (unspec? x) (eq? x '*))
      (define (spec? x) (not (unspec? x)))
      (define in-range
        (cond
          [(and (spec? from) (spec? to))
            (lambda (x) (< from x to))]
          [(and (unspec? from) (spec? to))
            (lambda (x) (< x to))]
          [(and (spec? from) (unspec? to))
            (lambda (x) (< from x))]
          [(and (unspec? from) (unspec? to))
            (lambda (x) #t)]))

      (lambda (str pos content top-matches)
        (let loop ([str str]
                   [pos pos]
                   [content content]
                   [repeats 0])

          (define (matches s end content cont)
            (define (mcont)
              (or (and cont (cont))
                  (loop s end content (add1 repeats))))
            (cond
              [(in-range? (add1 repeats))
                (top-matches s end content mcont)]
              [(in-range? repeats) #f]
              [else
                (loop s end content (add1 repeats))]))

          (subregexproc str pos content matches)))))

  )
