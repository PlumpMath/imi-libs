#!r6rs

(library (imi match basic)
  (export basic-matcher
          basic-matcher*)
  (import (rnrs))

  (define (basic-matcher pattern)
    (basic-matcher* pattern basic-matcher*))

  (define (basic-matcher* pattern matcher)
    (cond
      [(procedure? pattern)
       (pattern matcher)]
      [(pair? pattern)
       (pair-matcher pattern matcher)]
      [(vector? pattern)
       (vector-matcher pattern matcher)]
      [(symbol? pattern)
       (symbol-matcher pattern matcher)]
      [else
       (equal-matcher pattern matcher)]))

  (define (pair-matcher pattern matcher)
    (let ([match-car (matcher (car pattern) matcher)]
          [match-cdr (matcher (cdr pattern) matcher)])
      (lambda (expr)
        (and (pair? expr)
             (let ([mcar (match-car (car expr))]
                   [mcdr (match-cdr (cdr expr))])
               (and mcar
                    mcdr
                    (append (match-car (car expr))
                            (match-cdr (cdr expr)))))))))

  (define (vector-matcher pattern matcher)
    (let ([match-list (matcher (vector->list pattern) matcher)])
      (lambda (expr)
        (and (vector? expr)
             (match-list (vector->list expr))))))

  (define (symbol-matcher pattern matcher)
    (lambda (expr)
      (list (cons pattern expr))))

  (define (equal-matcher pattern matcher)
    (lambda (expr)
      (and (equal? expr pattern)
           '())))

  )

