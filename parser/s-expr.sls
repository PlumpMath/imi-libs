#!r6rs

(import (rnrs)
        (match)
        (imi sugar receive)
        (imi match utils)
        (imi io string))

#|
(define (sexpr-main-parser pos)
  (parse pos sexr-main))

(define (sexpr-list-parser pos)
  (read-char)
  (receive (len ls) (sexpr-pair-parser (+ 1 pos))
    (values (+ 1 len)
            ls)))

(define (sexpr-string-parser pos)
  (read-char)
  (receive (len str) (parse pos sexpr-string)
    (values (+ 1 len)
            str)))

(define (sexpr-any-parser pos)
  (parse pos sexpr-any))


(define sexpr-main
  `((#\space . skip)
    (#\( . ,sexpr-list-parser)
    (#\" . ,sexpr-string-parser)
    . ,sexpr-any-parser))


(define (sexpr-pair-parser pos)
  (receive (len0 elem) (sexpr-main-parser pos)
    (receive (len1 rest) (sexpr-pair-rest-parser (+ pos len0))
      (values (+ len0 len1)
              (cons pos
                    (cons elem rest))))))

(define (sexpr-pair-rest-parser pos)
  (parse pos sexpr-pair-rest))

(define (sexpr-pair-cdr-parser pos)
  (receive (len elem) (sexpr-main-parser pos)
    (receive (len0 null) (parse (+ pos len)
                                sexpr-pair-cdr-end)
      (values (+ len len0)
              elem))))

(define sexpr-pair-cdr-end
  `((#\space . skip)
    (#\) . ,(lambda (pos) '()))
    . ,(lambda (pos)
         (error 'sexpr-pair
                "expected ) here"
                (read-char) pos))))

(define sexpr-pair-rest
  `((#\space . skip)
    (#\.     . ,sexpr-pair-cdr-parser)
    . ,sexpr-pair-list-parser))
|#

(define-syntax synmap
  (syntax-rules ()
    [(_ proc ls cont)
     (synmap* proc ls () cont)]))

(define-syntax synmap*
  (syntax-rules ()
    [(_ proc (elem . rest) ls cont)
     (synmap* proc rest ((proc elem) . ls) cont)]
    [(_ proc () ls cont)
     (synreverse ls cont)]))

(define-syntax synimmap
  (syntax-rules ()
    [(_ proc ls cont)
     (synimmap* proc ls () cont)]))

(define-syntax synimmap*
  (syntax-rules ()
    [(_ proc (var0 . rest) ls cont)
     (synimmap* proc rest ((proc var0) . ls) cont)]
    [(_ proc () ls cont)
     (synreverse ls cont)]
    [(_ proc elem ls cont)
     (synreverse* (proc elem) ls cont)]))

(define-syntax synreverse
  (syntax-rules ()
    [(_ ls cont)
     (synreverse* () ls cont)]))

(define-syntax synreverse*
  (syntax-rules ()
    [(_ ls (elem . rest) cont)
     (synreverse* (elem . ls) rest cont)]
    [(_ ls () (cont ...))
     (cont ... ls)]))

(define-syntax test-map
  (syntax-rules ()
    [(_ proc ls)
     (synmap proc ls (quote))]))

(define-syntax list-let
  (syntax-rules ()
    [(_ vars ls body ...)
     (synimmap unquote vars (list-let* ls (body ...)))]))

(define-syntax list-let*
  (syntax-rules ()
    [(_ ls (body ...) vars)
     (begin (write 'vars) (newline)
            (match ls [vars body ...]))]))

(define lex-symbol
  '(('elim-whitespace ,@parse-symbol)
     (elim-whitespace
       (#\space elim-whitespace))
     (parse-symbol
       (,(eof #\space))
       (any parse-symbol))))


(define parse
  (case-lambda
    [(lex) (parse 0 lex)]
    [(pos lex)
     (list-let (start . rules) lex
       (parse* pos start rules))]))

(define (parse* pos rules def-rules)
  (list-let (pos . parsed)
    (fold-left (lambda (rule pos/parsed)
                 (list-let (pos . parsed) pos/parsed
                   (receive (pos pars) (parse-rule pos rule def-rules)
                     (cons pos
                           (append pars parsed)))))
               '()
               rules)
    (values pos parsed)))


(define (parse-rule pos rule def-rules)
  (cond
    [(qm-match (quote ,@?) rule)
     (list-let (ignore rule) rule
       (cons pos (parse-rule pos rule def-rules)))]
    [(qm-match (quote '?) rule)
     (list-let (ignore rule) rule
       (receive (pos parsed) (parse-rule pos rule def-rules)
         (values pos '())))]
    [(qm-match (quote ,?) rule)
     (list-let (ignore char) rule
       (let ([char-in (peek-char)])
         (if (char=? char char-in)
             (values pos '())
             (error 'parse-rule
                    "unexpected char"
                    char-in char))))]
    [(char? rule)
     (let ([char-in (read-char)])
       (if (char=? rule char-in)
           (values (+ 1 pos)
                   (list char-in))
           (error 'parse-rule
                  "unexpected char"
                  char-in rule)))]
    [(eq? rule 'eof)
     (let ([char-in (read-char)])
       (if (eof-object? char-in)
           (values (+ 1 pos)
                   '())
           (error 'parse-rule
                  "unexpected char"
                  char-in rule)))]
    [(symbol? rule)
     (let ([rule-def (assq rule def-rules)])
       (if rule-def
           (parse-rule pos (cdr rule-def) def-rules)
           (error 'parse-rule
                  "invalid rule name"
                  rule)))]
    [(list? rule)
     (let* ([char (peek-char)]
            [rules (find (lambda (rule)
                           (rule-match (car rule)
                                       char
                                       def-rules))
                         rule)])
       (if rules
           (parse* pos rules def-rules)
           (values pos '())))]
    [else
     (error 'parse-rule
            "invalid rule"
            rule)]))


(define (rule-match rule char def-rules)
  (cond
    [(or (qm-match (quote ,@?) rule)
         (qm-match (quote '?) rule)
         (qm-match (quote ,?) rule))
     (list-let (ignore rule) rule
       (rule-match rule char def-rules))]
    [(char? rule)
     (char=? rule char)]
    [(eq? rule 'eof)
     (eof-object? char)]
    [(symbol? rule)
     (let ([arule (assq rule def-rules)])
       (if arule
           (rule-match (cdr arule)
                       char 
                       def-rules)
           (error 'rule-match
                  "invalid rule"
                  rule)))]
    [(list? rule)
     (rule-match (car rule) char def-rules)]
    [else
     (error 'rule-match
            "invalid rule"
            rule)]))


(define lex-whitespace
  '((whitespace)
     (whitespace
       (#\space whitespace))))
