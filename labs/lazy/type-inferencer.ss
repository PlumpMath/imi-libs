(import (rnrs)
        (imi list set)
        )

(define (type-inference expr env appenv)
  (cond
    [(variable? expr)
     (type-inference-variable expr env appenv)]
    [(application? expr)
     (type-inference-application expr env appenv)]
    [else
     (type-inference-constant expr env appenv)]))


(define (tagged-list name)
  (lambda (expr) (and (pair? expr)
                      (eq? name (car expr)))))

(define variable?     symbol?)
(define application?  list?)



(define (type-inference-variable expr env appenv)
  (cond
    [(null? appenv) (list (free expr))]
    [(eq? expr (caar appenv))
     (list (cdar appenv))]
    [else
     (type-inference-variable expr env (cdr appenv))]))


(define (type-inference-application expr env appenv)
  (let ([proc (type-inference (car expr) env appenv)]
        [args (map (lambda (arg)
                     (type-inference arg env appenv))
                   (cdr expr))])
    (cond
      [(free? (car proc))
       (error 'type-inference-application
              "cannot inference free procedures yet"
              proc)]
      [(not (type-proc? (car proc)))
       (error 'type-inference-application
              "not a procedure"
              (car expr) proc)]
      [(type-for-all type-match? args (type-proc-args (car proc)))
       => (lambda (freetypes)
            (cons (type-proc-result (car proc))
                  (type-append (cdr proc)
                               freetypes)))]
      [else
       (error 'type-inference-application
              "invalid arguments for procedure"
              (car expr) proc
              (cdr expr) args)])))


(define (type-inference-constant expr env appenv)
  (exists (lambda (typedesc)
            (and (procedure? (cdr typedesc))
                 ((cdr typedesc)
                  expr)
                 (list (car typedesc))))
          env))




(define (type-for-all proc subtypes types)
  (cond
    [(and (null? subtypes)
          (null? types))
     '()]
    [(or (null? subtypes)
         (null? types))
     #f]
    [(proc (car subtypes)
           (car types))
     => (lambda (type)
          (cond
            [(type-for-all proc (cdr subtypes) (cdr types))
             => (lambda (freetypes)
                  (type-append type freetypes))]
            [else #f]))]
    [else #f]))


(define (type-match? subtype type)
  (cond
    [(free? (car subtype))
     (cond
       [(assq (free-var (car subtype))
              (cdr subtype))
        => (lambda (vartype)
             (type-match? (list (cdr vartype))
                          type))]
       [else
        (cons (cons (free-var (car subtype))
                    type)
              (cdr subtype))])]
    [(type-and? (car subtype))
     (and (for-all (lambda (type)
                     (type-match? subtype type))
                   (type-and-types type))
          (cdr subtype))]
    [(type-proc? (car subtype))
     (type-match-proc? subtype type)]
    [(eq? (car subtype) type)
     (cdr subtype)]
    [else #f]))


(define (type-match-proc? subtype type)
  (let ([proctype (car subtype)]
        [freevars (cdr subtype)])





(define (type-append ls0 ls1)
  (let ([names0 (map car ls0)]
        [names1 (map car ls1)])
    (append (map (lambda (name)
                   (cons name
                         (merge-and (cdr (assq name ls0))
                                    (cdr (assq name ls1)))))
                 (list-intersection symbol=? names0 names1))
            (map (lambda (name)
                   (assq name ls0))
                 (list-difference symbol=? names0 names1))
            (map (lambda (name)
                   (assq name ls1))
                 (list-difference symbol=? names1 names0)))))




(define (merge-and type0 type1)
  (let ([types (merge-same
                 (cond
                   [(and (type-and? type0)
                         (type-and? type1))
                    (append (cdr type0)
                                      (cdr type1))]
                   [(type-and? type0)
                    (cons type1 (cdr type0))]
                   [(type-and? type1)
                    (cons type0 (cdr type1))]
                   [else
                    (list type0 type1)]))])
    (if (null? (cdr types))
        (car types)
        (type-and types))))



(define (merge-same types)
  (cond
    [(null? types) '()]
    [else
     (cons (car types)
           (merge-same
             (remp (lambda (type)
                     (type-match? (list type)
                                  (car types)))
                   (cdr types))))]))




(define type-proc? (tagged-list 'procedure))

(define (type-proc res args)
  (cons* 'procedure res args))

(define (type-proc-args proc)
  (cddr proc))

(define (type-proc-result proc)
  (cadr proc))


(define free? (tagged-list 'free))

(define (free sth)
  (cons 'free sth))

(define (free-var free)
  (cdr free))


(define type-and? (tagged-list 'and))

(define (type-and ls)
  (cons 'and ls))

(define (type-and-types type)
  (cdr type))










(define env
  `((fixnum . ,fixnum?)
    (string . ,string?)
    (char   . ,char?)
    (null   . ,null?)
    ))




(define bifx (type-proc 'fixnum '(fixnum fixnum)))
(define cmpfx (type-proc 'boolean '(fixnum fixnum)))


(define appenv
  `((+    . ,bifx)
    (-    . ,bifx)
    (*    . ,bifx)
    (div  . ,bifx)
    (mod  . ,bifx)
    (=    . ,cmpfx)
    (<    . ,cmpfx)
    (>    . ,cmpfx)
    (null . null)
    ))



#!eof

(define (type-inference expr env)
  (cond
    [(if-expr? expr)
     (type-inference-if expr env)]
    [(application? expr)
     (type-inference-application expr env)]
    [else
     (type-inference-constant expr env)]))



(define if-expr?      (tagged-list 'if))
(define application?  list?)
(define variable?     symbol?)



(define (type-inference-if expr env)
  (type-inference (cadr expr) env)
  (let ([thentype (type-inference (caddr expr) env)]
        [elsetype (type-inference (cadddr expr) env)])
    (merge-or thentype elsetype)))


(define (type-inference-application expr env)
  (let ([proctype (type-inference (car expr) env)]
        [argtypes (map (lambda (arg)
                         (type-inference arg env))
                       (cdr expr))])
    (type-match-proc? proctype argtypes)))


(define (type-inference-constant expr env)
  (exists (lambda (typedesc)
            (and ((cdr typedesc)
                  expr)
                 (car typedesc)))
          env))




(define (type-match-proc? proctype argtypes)
  (let ([proctypes (if (type-or? proctype)
                       (filter type-proc? (cdr proctype))
                       (list proctype))])
    (cond
      [(null? proctype) #f]
      [(exists (lambda (type)
                 (type-for-all type-match? (type-proc-args type)))
               proctypes)
       => (lambda (proctype)
            (cons (type-proc-result (car proctype))
                  (cdr proctype)))]
      [else #f])))


(define (merge-or type0 type1)
  (cons (type-or (car type0)
                 (car type1))
        (let ([type0names (map car (cdr type0))]
              [type1names (map car (cdr type1))])
          (append
            (map (lambda (name)
                   (cons name
                         (merge-type-or (cdr (assq name (cdr type0)))
                                        (cdr (assq name (cdr type1))))))
                 (list-intersection symbol=? type0names type1names))
            (map (lambda (name)
                   (assq name (cdr type0)))
                 (list-difference symbol=? type0names type1names))
            (map (lambda (name)
                   (assq name (cdr type1)))
                 (list-difference symbol=? type1names type0names))))))




(define (type-for-all proc types)



#!eof

(define (type-inference expr env tenv)
  (cond
    [(if-expr? expr)
     (type-inference-if expr env tenv)]
    [(application? expr)
     (type-inference-application expr env tenv)]
    [(variable? expr)
     (type-inference-variable expr env tenv)]
    [else
     (type-inference-constant expr env tenv)]))



(define (application? expr)
  (list? expr))

(define (variable? expr)
  (symbol? expr))


(define (if-expr? expr)
  (and (pair? expr)
       (eq? 'if (car expr))))

(define (type-inference-if expr env)
  (type-inference (cadr expr) env)
  `(or ,(type-inference (caddr expr) env)
       ,(type-inference (cadddr expr) env)))


(define (type-inference-application expr env)
  (let ([proc (type-inference (car expr) env)]
        [args (map (lambda (arg)
                     (type-inference arg env))
                   (cdr expr))])
    (cond
      [(procedure? proc)
       (apply proc args)]
      [(for-all type-match?
                (type-lambda-args proc)
                args)
       (type-lambda-type proc)]
      [else
       (error 'type-inference-application
              "invalid arguments"
              args)])))


(define (type x)
  (cons 'type x))

(define (type? x)
  (or (and (pair? x)
           (eq? 'type (car x)))
      (type-lambda? x)
      (type-pair? x)))


(define-record-type type-lambda
  (fields args type))

(define-record-type type-pair
  (fields car cdr))

(define (comp p0 p1)
  (lambda args
    (p0 (apply p1 args))))


(define (type-inference-variable expr env tenv)
  (cond
    [(null? env) (error 'type-inference-variable
                        "unbound variable"
                        expr)]
    [(eq? expr (caar env))
     (cdar env)]
    [else
     (type-inference-variable expr (cdr env))]))


(define fixnum (type 'fixnum))
(define char (type 'char))
(define stringt (type 'string))
(define boolean (type 'boolean))
(define null (type 'null))


(define (type-inference-constant expr env tenv)
  (let ([const-type (map car (filter (lambda (typedesc)
                                       ((cdr typedesc)
                                        expr))
                                     tenv))])
    (cond
      [(null? const-type)
       (error 'type-inference-constant
              "invalid constant"
              expr)]
      [(null? (cdr const-type))
       (car const-type)]
      [else
       (cons 'and const-type)])))


(define (type-match? subtype type)
  (cond
    [(procedure? type)
     (type subtype)]
    [(type-or? type)
     (type-match-or? subtype type)]
    #;
    [(type-and? type)
     (type-match-and? subtype type)]
    [(type? type)
     (eq? subtype type)]
    [(list? type)
     (type-match-list? subtype type)]
    [else
     (error 'type-match?
            "not a type"
            type)]))


(define (type-or? type)
  (and (pair? type)
       (eq? 'or (car type))))

#;
(define (type-and? type)
  (and (pair? type)
       (eq? 'and (car type))))


(define (type-match-or? subtype type)
  (exists (lambda (type)
            (type-match? subtype type))
          (cdr type)))

;
;(define (type-match-and? subtype type)
;  (let loop ([types (cdr type)]
;             [res '()])
;    (cond
;      [(null? types) (cons subtype res)]
;      [(type-match? subtype (car type))
;       => (lambda (match)
;            (loop (cdr types)
;                  (merge-and (cdr match)
;                             res)))]
;      [else #f])))
;
;(define (merge-and match ls)
;  (cond
;    [(null? match) ls]
;    [(assq (caar match)
;           ls)
;     => (lambda (val)
;          (cons (cons (car val)
;                      (merge (cdar match)
;                             (cdr val)))
;                (merge-and (cdr match)
;                           (remp (lambda (elem)
;                                   (eq? (car val)
;                                        (car elem)))
;                                 ls))))]
;    [else
;     (cons (car match)
;           (merge-and (cdr match)
;                      ls))]))

(define (type-match-list? subtype type)
  (and (list? subtype)
       (eq? (car subtype)
            (car type))
       (cond
         [(type-match-all? (cdr subtype) (cdr type))
          => (lambda (match)
               (cons subtype match))]
         [else #f])))

(define (type-match-all? subtypes types)
  (cond
    [(null? subtypes)
     (and (null? types)
          '())]
    [(null? types) #f]
    [(type-match? (car subtypes)
                  (car types))
     => (lambda (match)
          (append (cdr match)
                  (type-match-all? (cdr subtypes)
                                   (cdr types))))]
    [else #f]))

(define fxarith (make-type-lambda (list fixnum fixnum) fixnum))
(define fxcmp (make-type-lambda (list fixnum fixnum) boolean))

(define std
  `((+ . ,fxarith)
    (- . ,fxarith)
    (* . ,fxarith)
    (div . ,fxarith)
    (mod . ,fxarith)
    (= . ,fxcmp)
    (< . ,fxcmp)
    (> . ,fxcmp)

    (null . ,null)

    (cons . ,(lambda (a d) (make-type-pair a d)))
    (car . ,(lambda (p) (if (type-pair? p)
                            (type-pair-car p)
                            (error 'car "invalid type" p))))
    (cdr . ,(lambda (p) (if (type-pair? p)
                            (type-pair-cdr p)
                            (error 'cdr "invalid type" p))))
    ))


(define tstd
  `((fixnum . ,fixnum?)
