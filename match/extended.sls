#!r6rs

(library (imi match extended)
  (export extended-matcher
          extended-matcher*)
  (import (rnrs)
          (imi sugar rec)
          (imi match basic)
          (imi match utils))

  (define (extended-matcher pattern)
    (extended-matcher* pattern extended-matcher*))

  (define (extended-matcher* pattern matcher)
    (cond
      [(quote? pattern)
       (quote-matcher pattern matcher)]
      [(label? pattern)
       (label-matcher pattern matcher)]
      [(one-of? pattern)
       (one-of-matcher pattern matcher)]
      [(neg? pattern)
       (neg-matcher pattern matcher)]
      [(string? pattern)
       (string-matcher pattern matcher)]
      [else
       (basic-matcher* pattern matcher)]))

  (define (quote? pattern)
    (qm-match '(quote ?)
              pattern))

  (define (quote-matcher pattern matcher)
    (let ([value (cadr pattern)])
      (lambda (expr)
        (and (equal? expr value)
             '()))))


  (define (label? pattern)
    (or (qm-match '(label ? ?)
                  pattern)
        (qm-match '($ ? ?)
                  pattern)))

  (define (label-matcher pattern matcher)
    (extend-label-matcher
      (cadr pattern)
      (caddr pattern)
      matcher))

  (define (extend-label-matcher label value original-matcher)
    (rec self-match
      (original-matcher
        value
        (lambda (pattern matcher)
          (if (label-ref? label pattern)
              (lambda (expr)
                (self-match expr))
              (original-matcher pattern matcher))))))

  (define (label-ref? label pattern)
    (or (qm-match `(label-ref ,label) pattern)
        (qm-match `(& ,label) pattern)))

  
  (define (one-of? pattern)
    (or (qm-match '(one-of . ?) pattern)
        (and (pair? pattern)
             (eq? (car pattern)
                  '?))))

  (define (one-of-matcher pattern matcher)
    (let ([matchers (map (lambda (elem)
                           (matcher elem matcher))
                         (cdr pattern))])
      (lambda (expr)
        (exists (lambda (matcher)
                  (matcher expr))
                matchers))))


  (define (neg? pattern)
    (or (qm-match '(neg ?) pattern)
        (qm-match '(! ?) pattern)))

  (define (neg-matcher pattern matcher)
    (let ([inner-matcher (matcher (cadr pattern) matcher)])
      (lambda (expr)
        (and (not (inner-matcher expr))
             '()))))


  (define (string-matcher pattern matcher)
    (let ([chars (string->list pattern)])
      (lambda (expr)
        (and (memq expr chars)
             '()))))




  )

