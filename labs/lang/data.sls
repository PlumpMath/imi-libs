#!r6rs

(library (imi labs lang data)
  (export lang-data?
          lang-data-symbol
          lang-data-string
          lang-data-number
          lang-data-pair
          lang-data-null

          scheme-data->lang-data
          lang-data->scheme-data
          
          lang-expr?
          lang-expr-variable
          lang-expr-quotation
          lang-expr-sequence
          lang-expr-set
          lang-expr-alternative
          lang-expr-abstraction
          lang-expr-application
          
          listof)
  (import (rnrs)
          (imi sugar rec)
          (imi labs lang datatype))

  (define-datatype lang-data?
    (lang-data-symbol (value symbol?))
    (lang-data-string (value string?))
    (lang-data-number (value number?))
    (lang-data-pair   (a lang-data?) (d lang-data?))
    (lang-data-null)
    )


  (define (scheme-data->lang-data data)
    (cond
      [(symbol? data) (lang-data-symbol data)]
      [(string? data) (lang-data-string data)]
      [(number? data) (lang-data-number data)]
      [(pair? data)
       (lang-data-pair (scheme-data->lang-data (car data))
                       (scheme-data->lang-data (cdr data)))]
      [(null? data)   (lang-data-null)]
      [else (error 'scheme-data->lang-data
                   "cannot convert this into lang-data"
                   data)]))


  (define (lang-data->scheme-data data)
    (cases data
      (lang-data-symbol (symb)
        symb)
      (lang-data-string (str)
        str)
      (lang-data-number (num)
        num)
      (lang-data-pair (a d)
        (cons (lang-data->scheme-data a)
              (lang-data->scheme-data d)))
      (lang-data-null ()
        '())
      ))



  (define-datatype lang-expr?
    (lang-expr-variable (name symbol?))
    (lang-expr-quotation (data lang-data?))
    (lang-expr-sequence (body (listof lang-expr?)))
    (lang-expr-set (var symbol?) (val lang-expr?))
    (lang-expr-alternative (c lang-expr?) (t lang-expr?) (f lang-expr?))
    (lang-expr-abstraction (vars (listof symbol?)) (body lang-expr?))
    (lang-expr-application (operator lang-expr?) (operands (listof lang-expr?)))
    )



  (define (listof pred)
    (rec listof-pred
      (lambda (x)
        (or (null? x)
            (and (pair? x)
                 (pred (car x))
                 (listof-pred (cdr x)))))))


  )

