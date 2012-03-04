#!r6rs

(library (imi labs playground webshop syntax-transformer)
  (export rules-transformer
          syntax-transform
          syntax-env-lookup
          syntax-env-extend
          syntax-bind-call)
  (import (rnrs)
          (imi labs playground webshop transform))

  (define (rules-transformer rules)
    (let ([rules-processors
            (map (lambda (rule)
                   (cons (destructor (car rule))
                         (constructor (cadr rule))))
                 rules)])
      (lambda (input)
        (or (exists (lambda (processor)
                      (let ([vars ((car processor) input)])
                        (and vars
                             ((cdr processor) vars))))
                    rules-processors)
            (error 'rules-transformer
                   "invalid syntax"
                   input rules)))))

  (define (syntax-transform input syntax-env)
    (if (pair? input)
        (case (car input)
          [(define define-class lambda let
            make instanceof send $send slot $slot
            quote quasiquote
            set! if begin)
           (special-transform input syntax-env)]
          [else
           (cond
             [(syntax-env-lookup syntax-env (car input))
              => (lambda (syntax-bind)
                   (syntax-transform
                     (syntax-bind-call syntax-bind input)
                     syntax-env))]
             [else (special-sequence-transform input syntax-env)])])
        input))

  (define (special-transform input syntax-env)
    (case (car input)
      [(define)       (special-define-transform (cadr input) (cddr input) syntax-env)]
      [(define-class) (special-class-transform (cadr input) (cddr input) syntax-env)]
      [(define-syntax) (special-syntax-transform input syntax-env)]
      [(lambda)       (special-lambda-transform (cadr input) (cddr input) syntax-env)]
      [(let)          (special-let-transform (cadr input) (cddr input) syntax-env)]
      [(make)         (special-make-transform (cadr input) (cddr input) syntax-env)]
      [(instanceof)   (special-instanceof-transform (cadr input) (caddr input) syntax-env)]
      [(send)         (special-send-transform (cadr input) (caddr input) (cdddr input) syntax-env)]
      [($send)        (special-static-send-transform (cadr input) (caddr input) (cdddr input) syntax-env)]
      [(slot)         (special-slot-transform input syntax-env)]
      [($slot)        (special-static-slot-transform input syntax-env)]
      [(quote)        (special-quote-transform (cadr input) syntax-env)]
      [(quasiquote)   (special-quasiquote-transform (cadr input) syntax-env)]
      [(set!)         (special-set-transform (cadr input) (caddr input) syntax-env)]
      [(if)           (special-if-transform (cadr input) (caddr input) (cadddr input) syntax-env)]
      [(begin)        (special-sequence-transform (cdr input) syntax-env)]
      [else           input]))


  (define (special-define-transform var body syntax-env)
    (cons* 'define
           var
           (special-sequence-transform body syntax-env)))

  (define (special-class-transform class body syntax-env)
    (cons* 'define-class
           class
           (map (lambda (slot)
                  (special-class-slot-transform slot syntax-env))
                body)))

  (define (special-class-slot-transform slot syntax-env)
    (cond
      [(eq? (cadr slot) 'static)
       (cons* (car slot)
              (cadr slot)
              (caddr slot)
              (special-sequence-transform (cdddr slot) syntax-env))]
      [else
       (cons* (car slot)
              (cadr slot)
              (special-sequence-transform (cddr slot) syntax-env))]))

  (define (special-syntax-transform input syntax-env)
    input)


  (define (special-lambda-transform n* e* syntax-env)
    (cons* 'lambda
           n*
           (special-sequence-transform e* syntax-env)))

  (define (special-let-transform b* e* syntax-env)
    (cons* 'let
           (map (lambda (b)
                  (special-let-bind-transform b syntax-env))
                b*)
           (special-sequence-transform e* syntax-env)))

  (define (special-let-bind-transform b syntax-env)
    (list (car b)
          (syntax-transform (cadr b) syntax-env)))


  (define (special-make-transform class vars syntax-env)
    (cons* 'make
           class
           (special-sequence-transform vars syntax-env)))

  (define (special-instanceof-transform var class syntax-env)
    (list 'instanceof
          (syntax-transform var syntax-env)
          (syntax-transform class syntax-env)))

  (define (special-send-transform obj meth e* syntax-env)
    (cons* 'send
           (syntax-transform obj syntax-env)
           meth
           (special-sequence-transform e* syntax-env)))

  (define (special-static-send-transform class meth e* syntax-env)
    (cons* '$send
           class
           meth
           (special-sequence-transform e* syntax-env)))

  (define (special-slot-transform input syntax-env)
    input)

  (define (special-static-slot-transform input syntax-env)
    input)

  (define (special-quote-transform e syntax-env)
    (list 'quote e))

  (define (special-quasiquote-transform e syntax-env)
    (list 'quasiquote
          (special-quasiquote-inner-transform e syntax-env)))

  (define (special-quasiquote-inner-transform e syntax-env)
    (if (pair? e)
        (if (eq? 'unquote (car e))
            (list 'unquote (syntax-transform (cadr e) syntax-env))
            (cons (special-quasiquote-inner-transform (car e) syntax-env)
                  (special-quasiquote-inner-transform (cdr e) syntax-env)))
        e))

  (define (special-set-transform n e syntax-env)
    (list 'set!
          n
          (syntax-transform e syntax-env)))

  (define (special-if-transform ec et ef syntax-env)
    (list 'if
          (syntax-transform ec syntax-env)
          (syntax-transform et syntax-env)
          (syntax-transform ef syntax-env)))

  (define (special-sequence-transform e* syntax-env)
    (if (null? e*)
        '()
        (cons (syntax-transform (car e*) syntax-env)
              (special-sequence-transform (cdr e*) syntax-env))))







  (define (syntax-env-lookup env var)
    (assq var env))

  (define (syntax-env-extend env var bind)
    (cons (cons var bind)
          env))

  (define (syntax-bind-call bind input)
    ((cdr bind) input))

  )

