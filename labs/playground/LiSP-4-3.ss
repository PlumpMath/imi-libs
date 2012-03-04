(import (rnrs))

(define (evaluate e r s k)
  (if (atom? e)
      (if (symbol? e)
          (evaluate-variable e r s k)
          (evaluate-quote e r s k))
      (case (car e)
        [(quote)  (evaluate-quote (cadr e) r s k)]
        [(if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r s k)]
        [(begin)  (evaluate-begin (cdr e) r s k)]
        [(set!)   (evaluate-set! (cadr e) (caddr e) r s k)]
        [(lambda) (evaluate-lambda (cadr e) (cddr e) r s k)]
        [else     (evaluate-application (car e) (cdr e) r s k)])))

(define (atom? e) (not (pair? e)))

(define (evaluate-if ec et ef r s k)
  (evaluate ec r s
    (lambda (v ss)
      (evaluate ((v 'boolify) et ef) r ss k))))

(define (evaluate-begin e* r s k)
  (if (pair? e*)
      (evaluate (car e*) r s
        (lambda (void ss)
          (evaluate-begin (cdr e*) r ss k)))
      (evaluate e* r s k)))

(define (r.init id)
  (error 'r.init "No binding for" id))

(define (update s a v)
  (lambda (aa)
    (if (eqv? aa a) v (s aa))))

(define (update* s a* v*)
  (if (pair? a*)
      (update* (update s (car a*) (car v*)) (cdr a*) (cdr v*))
      s))

(define (evaluate-variable n r s k)
  (k (s (r n)) s))

(define (evaluate-set! n e r s k)
  (evaluate e r s
    (lambda (v ss)
      (k v (update ss (r n) v)))))

(define (evaluate-application e e* r s k)
  (define (evaluate-arguments e* r s k)
    (if (pair? e*)
        (evaluate (car e*) r s
          (lambda (v ss)
            (evaluate-arguments (cdr e*) r ss
              (lambda (v* sss)
                (k (cons v v*) sss)))))
        (k '() s)))
  (evaluate e r s
    (lambda (f ss)
      (evaluate-arguments e* r ss
        (lambda (v* sss)
          (if (eq? (f 'type) 'function)
              ((f 'behavior) v* sss k)
              (error 'evaluate-application "Not a function" f)))))))

(define (evaluate-lambda n* e* r s k)
  (allocate 1 s
    (lambda (a* ss)
      (k (create-function
           (car a*)
           (lambda (v* s k)
             (if (= (length n*) (length v*))
                 (allocate (length n*) s
                   (lambda (a* ss)
                     (evaluate-begin e*
                                     (update* r n* a*)
                                     (update* ss a* v*)
                                     k)))
                 (error 'lambda "Incorrect arity" n* v*))))
         ss))))



(define (allocate n s q)
  (if (> n 0)
      (let ([a (new-location s)])
        (allocate (- n 1)
                  (expand-store a s)
                  (lambda (a* ss)
                    (q (cons a a*) ss))))
      (q '() s)))

(define (expand-store high-location s)
  (update s 0 high-location))

(define (new-location s)
  (+ 1 (s 0)))

(define s.init
  (expand-store 0 (lambda (a) (error 's.init "No such address allocated" a))))

(define (bool.true  x y) x)
(define (bool.false x y) y)

(define (invalid-msg msg)
  (error 'value "Invalid message" msg))

(define (getq x ls)
  (cond
    [(null? ls) (error 'getq "no such name" x)]
    [(eq? x (car ls))
     (cadr ls)]
    [else
     (getq x (cddr ls))]))

(define (make-value type bool . restmsgs)
  (lambda (msg)
    (case msg
      [(type)    type]
      [(boolify) bool]
      [else      (getq msg restmsgs)])))

(define the-empty-list
  (make-value 'null bool.true))

(define (create-boolean value)
  (let ([combinator (if value bool.true bool.false)])
    (make-value 'boolean combinator)))

(define (create-symbol n)
  (make-value 'symbol
              bool.true
              'name n))

(define (create-number num)
  (make-value 'number
              bool.true
              'value num))

(define (create-function tag behavior)
  (make-value 'function
              bool.true
              'tag      tag
              'behavior behavior))

(define (allocate-list v* s q)
  (define (consify v* q)
    (if (pair? v*)
        (consify (cdr v*) (lambda (v ss)
                            (allocate-pair (car v*) v ss q)))
        (q the-empty-list s)))
  (consify v* q))

(define (allocate-pair a d s q)
  (allocate 2 s
    (lambda (a* ss)
      (q (create-pair (car a*) (cadr a*))
         (update (update ss (car a*) a) (cadr a*) d)))))

(define (create-pair a d)
  (make-value 'pair
              bool.true
              'set-car (lambda (s v) (update s a v))
              'set-cdr (lambda (s v) (update s d v))
              'car     a
              'cdr     d))



(define (evaluate-quote v r s k)
  (cond
    [(null?   v) (k the-empty-list s)]
    [(number? v) (k (create-number v) s)]
    [(symbol? v) (k (create-symbol v) s)]
    [(pair?   v) (evaluate-quote-pair (car v) (cdr v) r s k)]
    [else        (error 'evaluate-quote "Invalid input - cannot convert" v)]))

(define (evaluate-quote-pair a d r s k)
  (evaluate-quote a r s
    (lambda (va ss)
      (evaluate-quote d r ss
        (lambda (vd sss)
          (allocate-pair va vd sss k))))))





(define s.global s.init)
(define r.global r.init)

(define-syntax definitial
  (syntax-rules ()
    [(definitial name value)
     (allocate 1 s.global
       (lambda (a* ss)
         (set! r.global (update r.global 'name (car a*)))
         (set! s.global (update ss (car a*) value))))]))

(define-syntax defprimitive
  (syntax-rules ()
    [(defprimitive name value arity)
     (definitial name
       (allocate 1 s.global
         (lambda (a* ss)
           (set! s.global (expand-store (car a*) ss))
           (create-function
             (car a*)
             (lambda (v* s k)
               (if (= arity (length v*))
                   (value v* s k)
                   (error 'name "Incorrect arity" arity v*)))))))]))


(define-syntax defpredicate
  (syntax-rules ()
    [(defpredicate name predtype)
     (defprimitive name
       (lambda (v* s k)
         (k (create-boolean (eq? ((car v*) 'type) 'predtype)) s))
       1)]))


(define-syntax defprimnum
  (syntax-rules ()
    [(defprimnumtest name result-type)
     (defprimitive name
       (lambda (v* s k)
         (if (and (eq? ((car v*) 'type) 'number)
                  (eq? ((cadr v*) 'type) 'number))
             (k (result-type (name ((car v*) 'value)
                                   ((cadr v*) 'value)))
                s)
             (error 'name "requires numbers" v*)))
       2)]))



(definitial t (create-boolean #t))
(definitial f (create-boolean #f))
(definitial nil the-empty-list)

(defprimnum =  create-boolean)
(defprimnum <  create-boolean)
(defprimnum >  create-boolean)
(defprimnum <= create-boolean)
(defprimnum >= create-boolean)
(defprimnum + create-number)
(defprimnum - create-number)
(defprimnum * create-number)
(defprimnum / create-number)

(defprimitive cons
  (lambda (v* s k)
    (allocate-pair (car v*) (cadr v*) s k))
  2)

(defprimitive car
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (k (s ((car v*) 'car)) s)
        (error 'car "not a pair" (car v*))))
  1)

(defprimitive cdr
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (k (s ((car v*) 'cdr)) s)
        (error 'cdr "not a pair" (car v*))))
  1)

(defprimitive set-car!
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (let ([pair (car v*)])
          (k pair ((pair 'set-car) s (cadr v*))))
        (error 'set-car! "not a pair" (car v*))))
  2)

(defprimitive set-cdr!
  (lambda (v* s k)
    (if (eq? ((car v*) 'type) 'pair)
        (let ([pair (car v*)])
          (k pair ((pair 'set-cdr) s (cadr v*))))
        (error 'set-cdr! "not a pair" (car v*))))
  2)

(defpredicate pair? pair)

(defprimitive eqv?
  (lambda (v* s k)
    (k (create-boolean
         (and (eq? ((car v*) 'type)
                   ((cadr v*) 'type))
              (case ((car v*) 'type)
                [(null) #t]
                [(boolean)
                 (((car v*) 'boolify)
                  (((cadr v*) 'boolify) #t #f)
                  (((cadr v*) 'boolify) #f #t))]
                [(symbol?)
                 (eq? ((car v*) 'name) ((cadr v*) 'name))]
                [(number)
                 (= ((car v*) 'value) ((cadr v*) 'value))]
                [(pair)
                 (and (= ((car v*) 'car) ((cadr v*) 'car))
                      (= ((car v*) 'cdr) ((cadr v*) 'cdr)))]
                [(function)
                 (= ((car v*) 'tag) ((cadr v*) 'tag))]
                [else #f])))
       s))
  2)


(define (interpreter)
  (define (toplevel s)
    (display ">> ")
    (evaluate (read)
              r.global
              s
              (lambda (v ss)
                (write (transcode-back v ss))
                (display " - ")
                (display (v 'type))
                (newline)
                (toplevel ss))))
  (toplevel s.global))


(define (transcode-back v s)
  (case (v 'type)
    [(null)     '()]
    [(boolean)  ((v 'boolify) #t #f)]
    [(symbol)   (v 'name)]
    [(string)   (v 'chars)]
    [(number)   (v 'value)]
    [(pair)     (cons (transcode-back (s (v 'car)) s)
                      (transcode-back (s (v 'cdr)) s))]
    [(function) v]
    [else       (error 'transcode-back "Unkown type" (v 'type))]))
