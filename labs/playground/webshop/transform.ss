(library (imi labs playground webshop transform)
  (export destructor
          symbol-destructor
          ellipsis-destructor
          pair-destructor
          equal-destructor

          clear-destructor
          clear-pair-destructor

          constructor
          symbol-constructor
          ellipsis-constructor
          pair-constructor
          insert-constructor

          merge-result
          get-ellipsis-one-var
          get-ellipsis-rest-var
          get-ellipsis-constructor

          collect-vars
          collect-symbol
          collect-ellipsis
          collect-pair
          collect-insert)
  (import (rnrs)
          (imi labs playground webshop syntax-types))


  (define (destructor pattern)
    (cond
      [($symbol? pattern)    (symbol-destructor pattern)]
      [(elliptical? pattern) (ellipsis-destructor pattern)]
      [($pair? pattern)      (pair-destructor pattern)]
      [else                  (equal-destructor pattern)]))


  (define (elliptical? pattern)
    (and ($pair? pattern)
         ($pair? ($cdr pattern))
         (eq? '... (cadr pattern))))


  (define (clear-destructor pattern)
    (cond
      [(symbol? pattern) (symbol-destructor pattern)]
      [(elliptical? pattern)
       (error 'clear-destructor "not a clear pattern" pattern)]
      [(pair? pattern)
       (if (symbol? (cdr pattern))
           (error 'clear-destructor "not a clear pattern" pattern)
           (clear-pair-destructor pattern))]
      [else (equal-destructor pattern)]))



  (define (symbol-destructor pattern)
    (lambda (input)
      (list (cons pattern input))))



  (define (ellipsis-destructor pattern)
    (let ([self (destructor (car pattern))]
          [rest (clear-destructor (cddr pattern))])
      (lambda (input)
        (let loop ([input input])
          (cond
            [(rest input)
             => (lambda (rest-result)
                  rest-result)]
            [(and ($pair? input)
                  (self ($car input)))
             => (lambda (subresult)
                  (let ([rest-result (loop ($cdr input))])
                    (and rest-result
                         (merge-result subresult rest-result))))]
            [else #f])))))

  (define (pair-destructor pattern)
    (let ([destructure-car (destructor (car pattern))]
          [destructure-cdr (destructor (cdr pattern))])
      (lambda (input)
        (and ($pair? input)
             (let ([result-car (destructure-car ($car input))]
                   [result-cdr (destructure-cdr ($cdr input))])
               (and result-car
                    result-cdr
                    (append result-car result-cdr)))))))

  (define (equal-destructor pattern)
    (lambda (input)
      (and (equal? input pattern)
           '())))

  (define (clear-pair-destructor pattern)
    (let ([destructure-car (destructor (car pattern))]
          [destructure-cdr (clear-destructor (cdr pattern))])
      (lambda (input)
        (and ($pair? input)
             (let ([result-car (destructure-car ($car input))]
                   [result-cdr (destructure-cdr ($cdr input))])
               (and result-car
                    result-cdr
                    (append result-car result-cdr)))))))



  (define (merge-result subresult result)
    (if (null? subresult)
        result
        (merge-result
          (cdr subresult)
          (let ([subres (car subresult)])
            (cond
              [(null? subresult) result]
              [(assq (car subres) result)
               => (lambda (res)
                    (cons (cons (car res)
                                (cons (cdr subres)
                                      (cdr res)))
                          (remp (lambda (r)
                                  (eq? (car r) (car res)))
                                result)))]
              [else
               (cons (cons (car subres)
                           (list (cdr subres)))
                     result)])))))





  (define (constructor pattern)
    (cond
      [(symbol? pattern)     (symbol-constructor pattern)]
      [(elliptical? pattern) (ellipsis-constructor pattern)]
      [(pair? pattern)       (pair-constructor pattern)]
      [else                  (insert-constructor pattern)]))


  (define (symbol-constructor pattern)
    (lambda (vars)
      (cond
        [(assq pattern vars) => cdr]
        [else pattern])))

  (define (pair-constructor pattern)
    (let ([car-construct (constructor (car pattern))]
          [cdr-construct (constructor (cdr pattern))])
      (lambda (vars)
        ($cons (car-construct vars)
               (cdr-construct vars)))))

  (define (insert-constructor pattern)
    (lambda (vars)
      pattern))

  (define (ellipsis-constructor pattern)
    (let* ([inner (car pattern)]
           [used-vars (collect-vars inner)]
           [construct (get-ellipsis-constructor inner)]
           [construct-rest (constructor (cddr pattern))])
      (lambda (vars)
        (let ([used (filter (lambda (var)
                              (memq (car var) used-vars))
                            vars)])
          (let loop ([used used])
            (cond
              [(exists (lambda (u) (null? (cdr u)))
                       used)
               (construct-rest vars)]
              [else
               ($cons (construct (get-ellipsis-one-var used))
                      (loop (get-ellipsis-rest-var used)))]))))))

  (define (get-ellipsis-one-var vars)
    (map (lambda (var)
           (cons (car var)
                 (cadr var)))
         vars))

  (define (get-ellipsis-rest-var vars)
    (map (lambda (var)
           (cons (car var)
                 (cddr var)))
         vars))


  (define (get-ellipsis-constructor inner)
    (cond
      [($symbol? inner)    (symbol-constructor inner)]
      [(elliptical? inner) (ellipsis-constructor inner)]
      [($pair? inner)      (pair-constructor inner)]
      [else                (insert-constructor inner)]))





  (define (collect-vars pattern)
    (cond
      [(symbol? pattern)     (collect-symbol pattern)]
      [(elliptical? pattern) (collect-ellipsis pattern)]
      [(pair? pattern)       (collect-pair pattern)]
      [else                  (collect-insert pattern)]))

  (define (collect-symbol pattern)
    (list pattern))

  (define (collect-ellipsis pattern)
    (append (collect-vars (car pattern))
            (collect-vars (cddr pattern))))

  (define (collect-pair pattern)
    (append (collect-vars (car pattern))
            (collect-vars (cdr pattern))))

  (define (collect-insert pattern)
    (list))

  )
