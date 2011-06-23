#!r6rs

(library (imi labs stream)
  (export counter
          doer
          endless
          shadow
          shadow*
          strmcurry
          strm->pair
          strms-divide
          strmap
          strmcut
          
          streach
          strmget
          strm->list)
  (import (rnrs)
          (imi sugar cut))

  (define counter
    (let loop ([i 0])
      (case-lambda
        [() (values i (loop (+ 1 i)))]
        [(offset)
         (values (+ i offset)
                 (loop (+ 1 i)))]
        [(offset step)
         (values (* step (+ i offset))
                 (loop (+ 1 i)))])))

  (define (doer proc)
    (lambda args
      (values (apply proc args)
              (doer proc))))

  (define (endless sth)
    (lambda ()
      (values sth (endless sth))))


  (define (shadow strm argstrm)
    (lambda args
      (let-values ([(arg argnext) (apply argstrm args)])
        (let-values ([(val strnext) (apply strm arg)])
          (values val
                  (shadow strnext
                          argnext))))))

  (define (shadow* strm . argstrms)
    (shadow strm (apply strmap
                        (lambda args args)
                        argstrms)))

  (define (strmcurry strm . args)
    (apply shadow* strm (map endless args)))

  (define (strm->pair strm . args)
    (call-with-values
      (lambda () (apply strm args))
      cons))

  (define (strms-divide strms . args)
    (let loop ([vals '()]
               [nexts '()]
               [strms strms])
      (if (null? strms)
          (values (reverse vals)
                  (reverse nexts))
          (call-with-values
            (lambda () (apply (car strms) args))
            (lambda (val next)
              (loop (cons val vals)
                    (cons next nexts)
                    (cdr strms)))))))

  (define (strmap proc . strms)
    (and (not (exists not strms))
         (lambda args
           (let-values ([(vals nexts)
                         (apply strms-divide strms args)])
             (values (apply proc vals)
                     (apply strmap proc nexts))))))

  (define (strmcut proc strm)
    (lambda args
      (let-values ([(val next) (apply strm args)])
        (values val
                (and (not (proc val))
                     (strmcut proc next))))))


  (define (streach proc . strms)
    (unless (exists not strms)
      (let-values ([(vals nexts) (strms-divide strms)])
        (apply proc vals)
        (apply streach proc nexts))))

  (define (strmget n strm)
    (cond
      [(zero? n) strm]
      [strm
       (let-values ([(val next) (strm)])
         (cons val
               (strmget (- n 1)
                        next)))]
      [else
       (error 'strmget
              "stream is shorter than n"
              strm n)]))

  (define (strm->list strm)
    (if strm
        (let-values ([(val next) (strm)])
          (cons val (strm->list next)))
        '()))

  )

