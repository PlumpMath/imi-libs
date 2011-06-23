(import (rnrs)
        (imi utils print))

(define (haskeval expr)
  (cond
    [(lambda? expr)
     (haskeval-lambda expr)]
    [(application? expr)
     (haskeval-app expr)]
    [(variable? expr)
     (haskeval-var expr)]
    [else
     (haskeval-data expr)]))




(define (lambda? expr)
  (and (pair? expr)
       (eq? 'lambda (car expr))))

(define (haskeval-lambda expr)
  (let ([args (cadr expr)]
        [body (caddr expr)])
    (let ([body-compiled (haskeval body)])
      (lambda (env)
        (let ([intern-proc (make-intern env args body-compiled)])
          (lambda () intern-proc))))))



(define-record-type intern
  (fields env args body))



(define (application? expr)
  (list? expr))

(define (haskeval-app expr)
  (let ([proc (haskeval (car expr))]
        [args (map haskeval (cdr expr))])
    (lambda (env)
      (let ([proc (proc env)])
        (lambda ()
          (let ([p (proc)])
            (cond
              [(hforeign? p)
               (hforeign-call p args)]
              [(intern? p)
               (intern-call p args)]
              [else
               (error 'haskeval-app
                      "not a callable value"
                      p)])))))))


(define (hforeign? proc)
  (and (pair? proc)
       (eq? (car proc)
            'special-mega-turtle-foreign-id)
       (procedure? (cdr proc))))

(define (hforeign-call proc args)
  (apply (cdr proc) args))



(define (intern-call proc args)
  (let ([env (extend-env/args (intern-env proc)
                              (intern-args proc)
                              args)]
        [body (intern-body proc)])
    (let ([body (body env)])
      (body))))


(define (extend-env/args env argnames argvalues)
  (cond
    [(null? argnames) env]
    [(pair? argnames)
     (extend-env/args (cons (cons (car argnames)
                                  (car argvalues))
                            env)
                      (cdr argnames)
                      (cdr argvalues))]
    [(symbol? argnames)
     (cons (cons argnames (lambda (env)
                            (fold-right (lambda (arg haskargs)
                                          (lambda ()
                                            (haskons (arg env)
                                                     haskargs)))
                                        '()
                                        argvalues)))
           env)]
    [else
     (error 'extend-env/args
            "invalid argument"
            argnames)]))



(define (variable? expr)
  (symbol? expr))

(define (haskeval-var expr)
  (lambda (env)
    (let loop ([search-env env])
      (cond
        [(null? search-env)
         (error 'haskeval-var
                "variable not bound"
                expr)]
        [(eq? expr (caar search-env))
         ((cdar search-env)
          env)]
        [else
         (loop (cdr search-env))]))))






(define (haskeval-data expr)
  (lambda (env)
    (lambda () expr)))


(define (scm->hask proc)
  (lambda (env)
    (lambda ()
      (cons 'special-mega-turtle-foreign-id
            (lambda args
              (apply proc (map (lambda (argproc)
                                 ((argproc env)))
                               args)))))))

(define (scm->lazy proc)
  (lambda (env)
    (lambda ()
      (cons 'special-mega-turtle-foreign-id
            (lambda args
              (apply proc (map (lambda (argproc)
                                 (argproc env))
                               args)))))))


(define (haskons a d)
  (cons a d))

(define (haskar pair)
  ((car (pair))))

(define (haskdr pair)
  ((cdr (pair))))


(define stdenv
  (append (map (lambda (var)
                 (cons (car var)
                       (scm->lazy
                         (cadr var))))
               `((cons ,haskons)
                 (car  ,haskar)
                 (cdr  ,haskdr)
                 ))
          (map (lambda (var)
                 (cons (car var)
                       (scm->hask
                         (cadr var))))
               `((=   ,fx=)
                 (<   ,fx<)
                 (>   ,fx>)
                 (+   ,fx+)
                 (-   ,fx-)
                 (*   ,fx*)
                 (mod ,mod)
                 (div ,div)
                 (null? ,null?)
                 ))
          '((null . '())
            )
          ))


(define (extend-env env . exprs)
  (if (null? exprs)
      env
      (apply extend-env
             (cons (cons (car exprs)
                         (haskeval (cadr exprs)))
                   env)
             (cddr exprs))))


(define extenv
  (extend-env stdenv
              'list '(lambda ls ls)
              'map '(lambda (proc ls)
                      (cons (proc (car ls))
                            (map proc (cdr ls))))
              ))

(define ownenv extenv)

(define (haskdef! name def)
  (set! ownenv (extend-env ownenf name def)))


(define (hasktest expr)
  (((haskeval expr) ownenv)))
