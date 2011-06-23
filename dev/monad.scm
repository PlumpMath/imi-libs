#!r6rs

(define (port data)
  (cons data 0))

(define (port-data port)
  (car port))

(define (port-pos port)
  (cdr port))

(define (port-char port)
  (string-ref (port-data port)
              (port-pos port)))


(define (with-port port io-operation)
  (io-monad-value
    (io-operation port)))


(define (io-monad port value)
  (cons port value))

(define (io-monad-port io-monad)
  (car io-monad))

(define (io-monad-value io-monad)
  (cdr io-monad))


(define (update port proc)
  (cons (port-data port)
        (proc (port-pos port))))

(define (ident x) x)
(define (set x) (lambda (y) x))


(define (get-char)
  (lambda (port)
    (io-monad (update port add1)
              (port-char port))))

(define (peek-char)
  (lambda (port)
    (io-monad (update port ident)
              (port-char port))))

(define (set-pos pos)
  (lambda (port)
    (io-monad (update port (set pos))
              (void))))

(define (eof?)
  (lambda (port)
    (io-monad port
              (>= (port-pos port)
                  (string-length (port-data port))))))

(define (io-bind io-operation io-proc)
  (lambda (port)
    (let ([io-monad (io-operation port)])
      ((io-proc (io-monad-value io-monad))
       (io-monad-port io-monad)))))

(define (io-return value)
  (lambda (port)
    (io-monad port value)))

(define io>=>
  (case-lambda
    [(io-proc) io-proc]
    [(io-operation . rest)
     (io-bind io-operation
              (apply io>=> rest))]))

(define-syntax io-do
  (syntax-rules (<-)
    [(_ io-return)
     io-return]
    [(_ [io-operation] rest ...)
     (io-bind
       io-operation
       (lambda (_)
         (io-do rest ...)))]
    [(_ [var <- io-operation] rest ...)
     (io-bind
       io-operation
       (lambda (var)
         (io-do rest ...)))]))
