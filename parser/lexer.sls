#!r6rs

(library (imi parser lexer)
  (export lex
          position?)
  (import (rnrs)
          (imi match utils)
          (imi sugar receive))


  (define (lex lex-desc)
    (receive (pos ls) (parse-rule (car lex-desc)
                                  0
                                  (cdr lex-desc))
      ls))


  (define (rule-parser char-rule
                       any-rule
                       eof-rule
                       ignore-rule
                       look-forward-rule
                       attach-pos-rule
                       attach-pos-name-rule
                       not-rule
                       error-rule
                       list-rule
                       vector-rule
                       symbol-rule)
    (lambda (rule . args)
      (apply (cond
               [(char? rule) char-rule]
               [(any-rule? rule) any-rule]
               [(eof-rule? rule) eof-rule]
               [(ignore-rule? rule) ignore-rule]
               [(look-forward-rule? rule) look-forward-rule]
               [(attach-pos-rule? rule) attach-pos-rule]
               [(attach-pos-name-rule? rule) attach-pos-name-rule]
               [(not-rule? rule) not-rule]
               [(error-rule? rule) error-rule]
               [(list? rule) list-rule]
               [(vector? rule) vector-rule]
               [(symbol? rule) symbol-rule]
               [else (error 'rule-parser
                            "invalid rule"
                            rule)])
             rule
             args)))

  (define (any-rule? rule)
    (eq? rule '%any))

  (define (eof-rule? rule)
    (eq? rule '%eof))

  (define (attach-pos-rule? rule)
    (qm-match (quote ,?) rule))

  (define (attach-pos-name-rule? rule)
    (qm-match (quote ,@?) rule))

  (define (ignore-rule? rule)
    (qm-match (quote '?) rule))

  (define (look-forward-rule? rule)
    (qm-match (quote `?) rule))

  (define (error-rule? rule)
    (qm-match '(%error ? ? . ?) rule))

  (define (not-rule? rule)
    (qm-match '(%not . ?) rule))


  (define (position? sth)
    (or (and (qm-match '(? ? ?) sth)
             (integer? (car sth))
             (integer? (cadr sth)))
        (and (pair? sth)
             (symbol? (car sth))
             (position? (cdr sth)))))


  (define (simple-rule-parser input-len pred?)
    (lambda (rule pos defs)
      (let ([input (read-char)])
        (if (pred? rule input defs)
            (values (+ input-len pos)
                    input)
            (error 'rule-parser
                   "input doesn't match rule"
                   input pred? rule pos)))))


  (define (matches-char-rule? char input defs)
    (and (char? input)
         (char=? char input)))

  (define parse-char-rule
    (simple-rule-parser 1 matches-char-rule?))



  (define (matches-any-rule? rule input defs)
    (not (eof-object? input)))

  (define parse-any-rule
    (simple-rule-parser 1 matches-any-rule?))



  (define (matches-eof-rule? rule input defs)
    (eof-object? input))

  (define parse-eof-rule
    (simple-rule-parser 0 matches-eof-rule?))



  (define (parse-ignore-rule rule pos defs)
    (receive (newpos result) (parse-rule (cadr rule) pos defs)
      (values newpos '())))

  (define (parse-look-forward-rule rule pos defs)
    (if (matches-rule? (cadr rule)
                       (peek-char)
                       defs)
        (values pos '())
        (error 'parse-look-forward-rule
               "input doesn't match"
               (peek-char) rule pos)))

  (define (parse-attach-pos-rule rule pos defs)
    (receive (newpos result) (parse-rule (cadr rule) pos defs)
      (values newpos
              (list pos newpos result))))

  (define (matches-dummy-rule? rule input defs)
    (matches-rule? (cadr rule) input defs))



  (define (parse-attach-pos-name-rule rule pos defs)
    (let ([name-rule (cadr rule)])
      (let ([name (car name-rule)]
            [inner-rule (cdr name-rule)])
        (receive (newpos result) (parse-rule inner-rule pos defs)
          (values newpos
                  (list name pos newpos result))))))

  (define (matches-attach-pos-name-rule? rule input defs)
    (let* ([name-rule (cadr rule)]
           [inner-rule (cdr name-rule)])
      (matches-rule? inner-rule input defs)))


  (define (parse-error-rule rule pos defs)
    (let ([who (cadr rule)]
          [msg (caddr rule)]
          [irritants (cdddr rule)])
      (apply error
             who
             msg
             (map (lambda (rule)
                    (receive (pos what) (parse-rule rule pos defs)
                      (list what 'at pos)))
                  irritants))))

  (define (matches-error-rule? rule input defs)
    #t)



  (define (parse-list-rule list-of-rules pos defs)
    (let* ([input (peek-char)]
           [matching-rule (find (lambda (rule)
                                  (matches-rule? rule input defs))
                                list-of-rules)])
      (if matching-rule
          (parse-rule matching-rule pos defs)
          (error 'parse-list-rule
                 "no matching rule"
                 input list-of-rules pos))))

  (define (matches-list-rule? list-of-rules input defs)
    (exists (lambda (rule)
              (matches-rule? rule input defs))
            list-of-rules))



  (define (parse-vector-rule vector-of-rules pos defs)
    (let loop ([i 0]
               [pos pos]
               [result '()])
      (if (= i (vector-length vector-of-rules))
          (values pos
                  (if (= 1 (length result))
                      (car result)
                      result))
          (receive (pos subresult) (parse-rule
                                     (vector-ref vector-of-rules i)
                                     pos
                                     defs)
            (loop (+ 1 i)
                  pos
                  (append result
                          (cond [(and (list? subresult)
                                      (not (position? subresult)))
                                 subresult]
                                [else (list subresult)])))))))

  (define (matches-vector-rule? vector-of-rules input defs)
    (or (zero? (vector-length vector-of-rules))
        (matches-rule? (vector-ref vector-of-rules 0)
                       input
                       defs)))



  (define (parse-symbol-rule name pos defs)
    (let ([rule (assq name defs)])
      (if rule
          (parse-rule (cdr rule) pos defs)
          (error 'parse-symbol-rule
                 "invalid rule name - not defined"
                 name defs pos))))

  (define (matches-symbol-rule? name input defs)
    (let ([rule (assq name defs)])
      (if rule
          (matches-rule? (cdr rule) input defs)
          (error 'matches-symbol-rule?
                 "invalid rule name - not defined"
                 name defs))))



  (define (simple-not-rule-parser pred?)
    (lambda (rule pos defs)
      (let ([input (read-char)])
        (if (not (pred? input))
            (values (+ 1 pos)
                    input)
            (error 'rule-parser
                   "input does not match"
                   input pred? `(%not . ,rule) pos)))))

  (define simple-not-allowed-parser
    (lambda (rule pos defs)
      (error 'not-rule-parser
             "rule is not allowed in a %not"
             rule pos)))


  (define parse-not-char-rule
    (simple-not-rule-parser matches-char-rule?))

  (define parse-not-any-rule
    (simple-not-rule-parser matches-any-rule?))

  (define parse-not-eof-rule
    (simple-not-rule-parser matches-eof-rule?))

  (define $parse-not-ignore-rule simple-not-allowed-parser)
  (define $parse-not-look-forward-rule simple-not-allowed-parser)
  (define $parse-not-attach-pos-rule simple-not-allowed-parser)
  (define $parse-not-attach-pos-name-rule simple-not-allowed-parser)
  (define $parse-not-not-rule simple-not-allowed-parser)
  (define $parse-not-error-rule simple-not-allowed-parser)

  (define (parse-not-list-rule list-of-rules pos defs)
    (let* ([input (read-char)]
           [matches? (for-all (lambda (rule)
                                (not (matches-rule? rule input defs)))
                              list-of-rules)])
      (if matches?
          (values (+ 1 pos)
                  input)
          (error 'parse-not-list-rule
                 "input does not match rule"
                 input `(%not . ,list-of-rules) pos))))

  (define $parse-not-vector-rule simple-not-allowed-parser)

  (define (parse-not-symbol-rule name pos defs)
    (let ([rule (assq name defs)])
      (if rule
          (parse-not-rule (cdr rule) pos defs)
          (error 'parse-not-symbol-rule
                 "undefined rule name"
                 name pos))))


  (define (matches-not-rule? rule input defs)
    (not (matches-rule? (cdr rule) input defs)))


  (define parse-not-rule-contents
    (rule-parser parse-not-char-rule
                 parse-not-any-rule
                 parse-not-eof-rule
                 $parse-not-ignore-rule
                 $parse-not-look-forward-rule
                 $parse-not-attach-pos-rule
                 $parse-not-attach-pos-name-rule
                 $parse-not-not-rule
                 $parse-not-error-rule
                 parse-not-list-rule
                 $parse-not-vector-rule
                 parse-not-symbol-rule))

  (define (parse-not-rule rule pos defs)
    (parse-not-rule-contents (cdr rule) pos defs))


  (define parse-rule
    (rule-parser parse-char-rule
                 parse-any-rule
                 parse-eof-rule
                 parse-ignore-rule
                 parse-look-forward-rule
                 parse-attach-pos-rule
                 parse-attach-pos-name-rule
                 parse-not-rule
                 parse-error-rule
                 parse-list-rule
                 parse-vector-rule
                 parse-symbol-rule))



  (define matches-rule?
    (rule-parser matches-char-rule?
                 matches-any-rule?
                 matches-eof-rule?
                 matches-dummy-rule? ;ignore-rule
                 matches-dummy-rule? ;look-forward-rule
                 matches-dummy-rule? ;attach-pos-rule
                 matches-attach-pos-name-rule?
                 matches-not-rule?
                 matches-error-rule?
                 matches-list-rule?
                 matches-vector-rule?
                 matches-symbol-rule?))

  )

