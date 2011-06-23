#!r6rs

(library (imi parser lexer-syntax)
  (export lexer
          position?
          quote
          quasiquote
          unquote-splicing)
  (import (rnrs)
          (imi sugar receive)
          (imi match utils)
          (imi parser lexer-syntax-compat))



  #|
  (define-syntax define-keyword
    (syntax-rules ()
      [(define-keyword keyword)
       (define-syntax keyword
         (lambda (stx)
           (error 'keyword
                  "invalid use of auxiliary keyword"
                  stx)))]))

  (define-keyword %any)
  (define-keyword %eof)
  (define-keyword %error)
  (define-keyword %not)
  |#



  (define-syntax lexer
    (lambda (stx)
      (syntax-case stx ()
        [(lexer start defs ...)
         #'(lambda ()
             (lexer-def-rule . defs) ...
             (lexer-def-ignore-rule . defs) ...
             (lexer-def-simple-rule . defs) ...
             (lexer-def-simple-ignore-rule . defs) ...

             (lexer-def-pred . defs) ...

             (lexer-rule start))])))


  (define-syntax lexer-pos
    (syntax-rules ()
      [(lexer-pos) (port-position (current-input-port))]))
         

  (define-syntax lexer-rule
    (lambda (stx)
      (syntax-case stx (quote quasiquote unquote unquote-splicing)
        [(lexer-rule ''rule)
         #'(lexer-rule 'rule)]
        [(lexer-rule '`rule)
         #'(lexer-rule `rule)]
        [(lexer-rule ',@(name . rule))
         #'(lexer-rule 'rule)]
        [(lexer-rule '(%error who msg irr ...))
         (%error? #'%error)
         #'(lexer-rule (%error who msg irr ...))]
        [(lexer-rule '(%not . rule))
         (%not? #'%not)
         #'(begin (lexer-rule (%not . rule))
                  '())]
        [(lexer-rule '%special)
         (exists (lambda (special?)
                   (special? #'%special))
                 (list %any? %eof?))
         #'(begin (lexer-rule %special)
                  '())]
        [(lexer-rule '(rules ...))
         #'(lexer-rule ('rules ...))]
        [(lexer-rule '#(rules ...))
         #'(lexer-rule #('rules ...))]
        [(lexer-rule 'name)
         (symbol? (syntax->datum #'name))
         (with-syntax ([ignore-rule (ignore-name #'name)])
           #'(lexer-rule ignore-rule))]
        [(lexer-rule 'rule)
         #'(begin (lexer-rule rule)
                  '())]

        [(lexer-rule `rule)
         #'(let ([input (peek-char)])
             (if (lexer-match? input rule)
                 '()
                 (lexer-std-error (lexer-pos) input '`rule)))]

        [(lexer-rule ,@(name . rule))
         ;#'(list 'name (lexer-pos) (lexer-flatten (lexer-rule rule)))]
         #'(list 'name (lexer-pos) (lexer-rule rule))]

        [(lexer-rule %eof)
         (%eof? #'%eof)
         #'(simple-lexer-rule 0 %eof)]

        [(lexer-rule %any)
         (%any? #'%any)
         #'(simple-lexer-rule 1 %any)]

        [(lexer-rule (%not . rule))
         (%not? #'%not)
         #'(simple-lexer-rule 1 (%not . rule))]

        [(lexer-rule rule)
         (not (or (pair? (syntax->datum #'rule))
                  (symbol? (syntax->datum #'rule))
                  (vector? (syntax->datum #'rule))))
         #'(simple-lexer-rule 1 rule)]

        [(lexer-rule (%error who msg irritants ...))
         (%error? #'%error)
         (with-syntax ([(irritantnames ...) (generate-temporaries #'(irritants ...))])
           #'(let* ([errorpos (lexer-pos)]
                    [irritantnames (lexer-rule irritants)] ...)
               (error 'who msg
                      (list 'at errorpos)
                      irritantnames ...)))]

        [(lexer-rule (rules ...))
         #'(let ([input (peek-char)])
             (cond
               [(lexer-match? input rules)
                (lexer-rule-simple rules)]
               ...
               [else (lexer-std-error (lexer-pos) input '(rules ...))]))]

        [(lexer-rule #())
         #''()]
        [(lexer-rule #(rule0))
         #'(lexer-rule rule0)]
        [(lexer-rule #(rules ...))
         (with-syntax ([(results ...) (generate-temporaries #'(rules ...))])
           #'(let* ([results (lexer-rule rules)] ...)
               (let ([res (remp null? (list results ...))])
                 (append-res res))))]


        [(lexer-rule name)
         (symbol? (syntax->datum #'name))
         (with-syntax ([rule (rule-name #'name)])
           #'(rule))]
        )))


  (define-syntax lexer-rule-simple
    (lambda (stx)
      (syntax-case stx (quote quasiquote unquote unquote-splicing)
        [(lexer-rule-simple ''rule)
         #'(lexer-rule-simple 'rule)]
        [(lexer-rule-simple '`rule)
         #'(lexer-rule-simple `rule)]
        [(lexer-rule-simple ',@(name . rule))
         #'(lexer-rule-simple 'rule)]
        [(lexer-rule-simple '(%error who msg irr ...))
         (%error? #'%error)
         #'(lexer-rule-simple (%error who msg irr ...))]
        [(lexer-rule-simple '(%not . rule))
         (%not? #'%not)
         #''()]
        [(lexer-rule-simple '%special)
         (exists (lambda (special?)
                   (special? #'%special))
                 (list %any? %eof?))
         #'(begin (lexer-rule-simple %special)
                  '())]
        [(lexer-rule-simple '(rules ...))
         #'(lexer-rule-simple ('rules ...))]
        [(lexer-rule-simple '#(rules ...))
         #'(lexer-rule-simple #('rules ...))]
        [(lexer-rule-simple 'name)
         (symbol? (syntax->datum #'name))
         (with-syntax ([ignore-rule (ignore-name #'name)])
           #'(lexer-rule-simple ignore-rule))]
        [(lexer-rule-simple 'rule)
         #'(begin (lexer-rule-simple rule)
                  '())]

        [(lexer-rule-simple `rule)
         #''()]

        [(lexer-rule-simple ,@(name . rule))
         ;#'(list 'name (lexer-pos) (lexer-flatten (lexer-rule-simple rule)))]
         #'(list 'name (lexer-pos) (lexer-rule-simple rule))]

        [(lexer-rule-simple %eof)
         (%eof? #'%eof)
         #'(read-char)]

        [(lexer-rule-simple %any)
         (%any? #'%any)
         #'(read-char)]

        [(lexer-rule-simple (%not . rule))
         (%not? #'%not)
         #'(read-char)]

        [(lexer-rule-simple rule)
         (not (or (pair? (syntax->datum #'rule))
                  (symbol? (syntax->datum #'rule))
                  (vector? (syntax->datum #'rule))))
         #'(read-char)]

        [(lexer-rule-simple (%error who msg irritants ...))
         (%error? #'%error)
         (with-syntax ([(irritantnames ...) (generate-temporaries #'(irritants ...))])
           #'(let* ([errorpos (lexer-pos)]
                    [irritantnames (lexer-rule-simple irritants)] ...)
               (error 'who msg
                      (list 'at errorpos)
                      irritantnames ...)))]

        [(lexer-rule-simple (rules ...))
         #'(let ([input (peek-char)])
             (cond
               [(lexer-match? input rules)
                (lexer-rule-simple rules)]
               ...
               [else (lexer-std-error (lexer-pos) input '(rules ...))]))]

        [(lexer-rule-simple #())
         #''()]
        [(lexer-rule-simple #(rule0))
         #'(lexer-rule-simple rule0)]
        [(lexer-rule-simple #(rule0 rules ...))
         (with-syntax ([(results ...) (generate-temporaries #'(rules ...))])
           #'(let* ([result0 (lexer-rule-simple rule0)]
                    [results (lexer-rule rules)] ...)
               (let ([res (remp null? (list result0 results ...))])
                 (append-res res))))]


        [(lexer-rule-simple name)
         (symbol? (syntax->datum #'name))
         (with-syntax ([rule (simple-name #'name)])
           #'(lexer-rule rule))]
        )))

  (define-syntax lexer-std-error
    (syntax-rules ()
      [(_ pos input rule)
       (error 'lexer-rule
              "input doesn't match"
              input rule pos)]))

  (define-syntax simple-lexer-rule
    (syntax-rules ()
      [(_ len rule)
       (let ([input (read-char)])
         (if (lexer-match? input rule)
             input
             (lexer-std-error (lexer-pos) input rule)))]))

  (define (position? sth)
    (and (qm-match '(? ? ?) sth)
         (symbol? (car sth))
         (integer? (cadr sth))))

  (define (simple? sth)
    (or (not (pair? sth))
        (position? sth)))



  (define (lexer-flatten ls)
    (if (simple? ls)
        (list ls)
        (apply append
               (map lexer-flatten
                    (remp null? ls)))))

  (define (append-res res)
    (cond
      [(null? res) '()]
      [(null? (cdr res))
       (if (simple? (car res))
           res
           (car res))]
      [else
       (cons (car res)
             (append-res (cdr res)))]))



  (define-syntax lexer-match?
    (lambda (stx)
      (syntax-case stx (quote quasiquote unquote unquote-splicing)
        [(lexer-match? input 'rule)
         #'(lexer-match? input rule)]
        [(lexer-match? input `rule)
         #'(lexer-match? input rule)]
        [(lexer-match? input ,@(name . rule))
         #'(lexer-match? input rule)]
        [(lexer-match? input (%not . rule))
         (%not? #'%not)
         #'(not (lexer-match? input rule))]
        [(lexer-match? input (%error who msg . irritants))
         (%error? #'%error)
         #'#t]
        [(lexer-match? input char)
         (char? (syntax->datum #'char))
         #'(and (char? input)
                (char=? input char))]
        [(lexer-match? input %any)
         (%any? #'%any)
         #'(not (eof-object? input))]
        [(lexer-match? input %eof)
         (%eof? #'%eof)
         #'(eof-object? input)]
        [(lexer-match? input name)
         (symbol? (syntax->datum #'name))
         (with-syntax ([rulepred (rule-pred #'name)])
           #'(rulepred input))]
        [(lexer-match? input (rules ...))
         #'(or (lexer-match? input rules)
               ...)]
        [(lexer-match? input #())
         #'#t]
        [(lexer-match? input #(rule0 rest ...))
         #'(lexer-match? input rule0)]
        )))





  (define-syntax lexer-def-rule
    (lambda (stx)
      (syntax-case stx ()
        [(lexer-def-rule name . rule)
         (with-syntax ([rulename (rule-name #'name)])
           #'(define (rulename) (lexer-rule rule)))])))

  (define-syntax lexer-def-ignore-rule
    (lambda (stx)
      (syntax-case stx ()
        [(lexer-def-ignore-rule name . rule)
         (with-syntax ([ignorename (ignore-name #'name)])
           #'(lexer-def-rule ignorename . 'rule))])))

  (define-syntax lexer-def-simple-rule
    (lambda (stx)
      (syntax-case stx ()
        [(lexer-def-simple-rule name . rule)
         (with-syntax ([simplename (rule-name (simple-name #'name))])
           #'(define (simplename) (lexer-rule-simple rule)))])))

  (define-syntax lexer-def-simple-ignore-rule
    (lambda (stx)
      (syntax-case stx ()
        [(lexer-def-simple-ignore-rule name . rule)
         (with-syntax ([ignorename (ignore-name #'name)])
           #'(lexer-def-simple-rule ignorename . 'rule))])))


  (define-syntax lexer-def-pred
    (lambda (stx)
      (syntax-case stx ()
        [(lexer-def-pred name . rule)
         (with-syntax ([rulepred (rule-pred #'name)])
           #'(define (rulepred input) (lexer-match? input rule)))])))

  )

