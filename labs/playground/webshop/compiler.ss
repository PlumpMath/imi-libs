(library (imi labs playground webshop compiler)
  (export eat-up-whitespace
          read-comment

          read-file

          compile-file
          compile-instructions
          compile-instruction
          compile-special-definition
          compile-use
          compile-simple


          process-syntax


          analyse-file
          analyse-instructions
          analyse-instruction
          analyse-alias
          analyse-operator
          analyse-comparator
          analyse-special
          analyse-use
          analyse-simple)
  (import (rnrs)
          (imi sugar receive)
          (imi io string)
          (imi list processing)
          (imi labs playground webshop php)
          (imi labs playground webshop syntax-transformer))


  (define (eat-up-whitespace)
    (let loop ([str '()])
      (case (peek-char)
        [(#\space #\newline #\tab)
         (loop (cons (read-char)
                     str))]
        [(#\;)
         (loop (append (read-comment)
                       str))]
        [else (list->string (reverse str))])))


  (define (read-comment)
    (let loop ([str (string->list "//")])
      (case (peek-char)
        [(#\newline) str]
        [else (loop (cons (read-char)
                          str))])))


  (define (read-file file)
    (with-input-from-file file
      (lambda ()
        (let loop ([e* '()]
                   [white* '()])
          (let* ([white (eat-up-whitespace)]
                 [e (read)])
            (if (eof-object? e)
                (values (reverse e*)
                        (reverse (cons white white*)))
                (loop (cons e e*)
                      (cons white white*))))))))



  (define (extend/defs r defs)
    (cons (car r)
          (append defs (cdr r))))





  (define (compile-file file)
    (receive (e* white*) (read-file file)
      (let* ([e* (process-syntax e*)]
             [defs (analyse-instructions e*)]
             [r.file (extend/defs r.global defs)]
             [c* (compile-instructions e* r.file)])
        (list-intertwine white* c*))))


  (define (compile-instructions e* r)
    (map (lambda (e)
           (compile-instruction e r))
         e*))



  (define (compile-instruction e r)
    (if (pair? e)
        (case (car e)
          [(define-alias define-operator define-comparator define-special define-syntax)
           (compile-special-definition e)]
          [(use)
           (compile-use (cadr e))]
          [else (compile-simple e r)])
        (compile-simple e r)))




  (define (compile-special-definition e)
    (list "// " (with-output-to-string
                  (lambda () (write e)))))

  (define (compile-use file)
    (let ([filename (cond
                      [(symbol? file)
                       (symbol->string file)]
                      [(string? file)
                       file]
                      [else (error 'compile-use
                                   "invalid filename"
                                   file)])])
      (string-append "require_once (\"" filename ".php\"); ")))



  (define (compile-simple e r)
    (receive (pre* c) (compile e r (position sequence))
      (cons 'group
            (append pre* (list c)))))





  (define (process-syntax e*)
    (receive (e* syntax-env) (process-syntax-definitions e*)
      (map (lambda (e)
             (syntax-transform e syntax-env))
           e*)))

  (define (process-syntax-definitions e*)
    (let loop ([e* e*]
               [env '()]
               [new-e* '()])
      (cond
        [(null? e*)
         (values (reverse new-e*)
                 env)]
        [(pair? (car e*))
         (case (car (car e*))
           [(define-syntax)
            (loop (cdr e*)
                  (process-syntax-definition (car e*) env)
                  (cons (car e*)
                        new-e*))]
           [else
            (loop (cdr e*)
                  env
                  (cons (car e*)
                        new-e*))])]
        [else
         (loop (cdr e*)
               env
               (cons (car e*)
                     new-e*))])))


  (define (process-syntax-definition e syntax-env)
    (let ([var (cadr e)]
          [rules (cddr e)])
      (syntax-env-extend syntax-env var
        (rules-transformer rules))))





  (define (analyse-file file)
    (receive (e* white*) (read-file file)
      (analyse-instructions (process-syntax e*))))

  (define (analyse-instructions e*)
    (fold-left (lambda (defs e)
                 (append (analyse-instruction e defs)
                         defs))
               '()
               e*))

  (define (analyse-instruction e defs)
    (if (pair? e)
        (case (car e)
          [(define-alias)
           (analyse-alias e)]
          [(define-operator)
           (analyse-operator e)]
          [(define-comparator)
           (analyse-comparator e)]
          [(define-special)
           (analyse-special e)]
          [(use)
           (analyse-use (cadr e))]
          [(define define-class)
           (analyse-simple e defs)]
          [else '()])
        '()))


  (define (analyse-alias e)
    (let ([name (cadr e)]
          [type (caddr e)]
          [alias (cadddr e)])
      (list `(,name (,type ,alias)))))

  (define (analyse-operator e)
    (list (operator (cadr e) (caddr e))))

  (define (analyse-comparator e)
    (list (comparator (cadr e) (caddr e))))

  (define (analyse-special e)
    (list (keywordop (cadr e) (caddr e))))


  (define (analyse-use file)
    (let ([filename (cond
                      [(symbol? file)
                       (symbol->string file)]
                      [(string? file)
                       file]
                      [else (error 'analyse-use
                                   "invalid filename"
                                   file)])])
      (analyse-file (string-append filename ".scm"))))


  (define (analyse-simple e defs)
    (let ([r (extend/defs r.global defs)])
      (list (car (cdr (analyse e r))))))

  )
