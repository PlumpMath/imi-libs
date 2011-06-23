#!r6rs

(library (imi labs lang parser)
  (export parse
          parse-variable
          parse-quotation
          parse-sequence
          parse-set
          parse-alternative
          parse-abstraction
          parse-application)
  (import (rnrs)
          (imi labs lang data)
          (imi labs lang datatype))


  (define (atom? expr) (not (pair? expr)))

  (define (parse expr)
    (if (atom? expr)
        (if (symbol? expr) (parse-variable expr)
                           (parse-quotation expr))
        (case (car expr)
          [(quote)         (parse-quotation (cadr expr))]
          [(begin)         (parse-sequence (cdr expr))]
          [(set!)          (parse-set (cadr expr) (caddr expr))]
          [(if)            (parse-alternative (cadr expr) (caddr expr) (cadddr expr))]
          [(lambda)        (parse-abstraction (cadr expr) (cddr expr))]
          [else            (parse-application (car expr) (cdr expr))])))

  (define (parse-variable var)
    (lang-expr-variable var))

  (define (parse-quotation data)
    (lang-expr-quotation (scheme-data->lang-data data)))

  (define (parse-sequence expr*)
    (lang-expr-sequence
      (map parse expr*)))

  (define (parse-set var body)
    (lang-expr-set var (parse body)))

  (define (parse-alternative cond-expr true-expr false-expr)
    (lang-expr-alternative (parse cond-expr)
                           (parse true-expr)
                           (parse false-expr)))

  (define (parse-abstraction args body)
    (lang-expr-abstraction args (parse-sequence body)))

  (define (parse-application operator operands)
    (lang-expr-application
      (parse operator)
      (map parse operands)))

  )

