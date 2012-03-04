(import (rnrs))


;;; e  - expression
;;; v  - value
;;; n  - identifier (name)
;;; r  - environment
;;; s  - memory (store)
;;; sr - frame (store environment)
;;; a  - address
;;; k  - continuation

(define (meaning e r)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r)
                      (meaning-quotation e r))
      (case (car e)
        [(quote)      (meaning-quotation (cadr e) r)]
        [(begin)      (meaning-sequence (cdr e) r)]
        [(set!)       (meaning-set! (cadr e) (caddr e) r)]
        [(if)         (meaning-alternative (cadr e) (caddr e) (cadddr e) r)]
        [(lambda)     (meaning-abstraction (cadr e) (cddr e) r)]
        [else         (meaning-application (car e) (cdr e) r)])))

(define (atom? e) (not (pair? e)))


(define (meaning-reference e r)
  (let ([ar (lookup e r)])
    (lambda (s sr k)
      (let ([x (get s (ref sr ar))])
        (if (eq? x 0)
            (error e "undefined value")
            (k x s))))))


(define (meaning-quotation e r)
  (lambda (s sr k)
    (allocate-value e s sr r k)))




(define (meaning-sequence e* r)
  (cond
    [(null? e*)
     (error 'meaning-sequence
            "empty sequence"
            e*)]
    [(null? (cdr e*))
     (meaning (car e*) r)]
    [else
     (let ([m (meaning (car e*) r)]
           [m* (meaning-sequence (cdr e*) r)])
       (lambda (s sr k)
         (m s sr (lambda (v s1)
                   (m* s1 sr k)))))]))


(define (meaning-set! n e r)
  (let ([m (meaning e r)]
        [ar (lookup n r)])
    (lambda (s sr k)
      (m s sr (lambda (a s1)
                (k a (set s1 (ref sr ar) a)))))))




(define (meaning-alternative ec et ef r)
  (let ([mc (meaning ec r)]
        [mt (meaning et r)]
        [mf (meaning ef r)])
    (lambda (s sr k)
      (mc s sr (lambda (ac s1)
                 (if (not (eq? 'false (car ac)))
                     (mt s1 sr k)
                     (mf s1 sr k)))))))



(define (meaning-abstraction n* e* r)
  (let* ([rr (extend r n*)]
         [arity (length n*)]
         [m* (meaning-sequence e* rr)])
    (lambda (s sr.lexical k)
      (create-abstraction
        (lambda (a* s1 k1)
          (if (= arity (length a*))
              (allocate-frame s1 sr.lexical r a*
                (lambda (sr.abstraction s2)
                  (m* s2 sr.abstraction k1)))
              (error 'abstraction
                     "incorrect arity"
                     a* n*)))
         s
         sr.lexical
         r
         k))))



(define (meaning-application e e* r)
  (let ([mf (meaning e r)]
        [m* (meaning* e* r)])
    (lambda (s sr k)
      (mf s sr
        (lambda (af s1)
          (if (eq? 'abstraction (car af))
              (let ([f (get s1 (cdr af))])
                (if (procedure? f)
                    (m* s1 sr (lambda (a* s2)
                                (f a* s2 k)))
                    (error 'application
                           "not a procedure"
                           e f)))
              (error 'application
                     "not an abstraction"
                     af)))))))


(define (meaning* e* r)
  (if (null? e*)
      (lambda (s sr k)
        (k '() s))
      (let ([m (meaning (car e*) r)]
            [m* (meaning* (cdr e*) r)])
        (lambda (s sr k)
          (m* s sr (lambda (a* s1)
                     (m s1 sr (lambda (a s2)
                                (k (cons a a*) s2)))))))))




(define (allocate-frame s sr r a* q)
  (allocate s sr r (length a*)
    (lambda (a.base s1)
      (q (cons sr a.base)
         (set* s1 (range a.base (length a*))
                  a*)))))

(define (range start len)
  (if (zero? len)
      '()
      (cons start (range (+ start 1)
                         (- len 1)))))

(define (extend r n*)
  (cons r n*))



(define (ref sr ar)
  (let layer ([level (car ar)]
              [sr sr])
    (if (= 0 level)
        (+ (cdr sr) (cdr ar))
        (layer (- level 1)
               (car sr)))))

(define (lookup n r)
  (let lookframe ([r r]
                  [level 0])
    (if (null? r)
        (error 'lookup
               "unbound identifier"
               n)
        (let inframe ([n* (cdr r)]
                      [x 0])
          (cond
            [(null? n*)
             (lookframe (car r) (+ 1 level))]
            [(eq? n (car n*))
             (cons level x)]
            [else
             (inframe (cdr n*)
                      (+ 1 x))])))))



(define (get s a)
  (let ([x (assq a (cdr s))])
    (if x (cdr x)
          (error 'get "invalid memory address" a s))))

(define (set s a v)
  (cons 
    (car s)
    (let loop ([s (cdr s)])
      (if (eq? a (caar s))
          (cons (cons (caar s)
                      v)
                (cdr s))
          (cons (car s)
                (loop (cdr s)))))))

(define (set* s a* v*)
  (if (and (pair? a*)
           (pair? v*))
      (set* (set s (car a*) (car v*))
            (cdr a*)
            (cdr v*))
      s))


(define (allocate-value v s sr r k)
  (cond
    [(number? v)
     (create-number v s sr r k)]
    [(pair? v)
     (allocate-value (car v) s sr r
       (lambda (aa s1)
         (allocate-value (cdr v) s1 sr r
           (lambda (ad s2)
             (create-pair aa ad s2 sr r k)))))]
    [(null? v)
     (create-null s sr r k)]
    [(boolean? v)
     ((if v create-true
            create-false)
      s sr r k)]
    [(procedure? v)
     (create-abstraction v s sr r k)]
    [else
     (error 'allocate-value
            "invalid type"
            v)]))


(define (allocate s sr r size q)
  (if (and sr r
           (> (+ size (length s))
              *max-alloc*))
      (allocate (gc s sr r)
                sr r size q)
      (case size
        [(0) (q -1 s)]
        [(1) (let ([a (car s)])
               (q a
                  (cons* (+ 1 a)
                         (cons a 0)
                         (cdr s))))]
        [else
         (let ([a (car s)])
           (allocate 
             (cons* (+ 1 a)
                    (cons a 0)
                    (cdr s))
             sr r
             (- size 1)
             (lambda (aa ss)
               (q a ss))))])))



(define *max-alloc* 500)

(define (gc s sr r)
  (display "collecting ...") (newline)
  (let ([needed (gc-collect sr r)])
    (display "need ") (write needed) (newline)
    (display "before ") (write (length s))
    (let ([s.collected
           (cons 
             (car s)
             (fold-left (lambda (ss need)
                          (let ([ptr (assq need (cdr s))])
                            (merge (merge ss ptr)
                                   (assq (cddr ptr)
                                         (cdr s)))))
                        '()
                        needed))])
      (display " after ") (write (length s.collected))
      (newline)
      s.collected)))

(define (gc-collect sr r)
  (if (null? sr)
      '()
      (append (range (cdr sr) (length (cdr r)))
              (gc-collect (car sr) (car r)))))

(define (merge s v)
  (if (assq v s)
      s
      (cons v s)))







(define (create-number v s sr r k)
  (allocate s sr r 1
    (lambda (a s1)
      (k (cons 'number a)
         (set s1 a v)))))

(define (create-pair a d s sr r k)
  (allocate s sr r 2
    (lambda (ap s1)
      (k (cons 'pair ap)
         (set* s1 (list ap (+ ap 1))
                  (list a  d))))))

(define (create-null s sr r k)
  (allocate s sr r 0
    (lambda (a s1)
      (k (cons 'null a)
         s1))))

(define (create-true s sr r k)
  (allocate s sr r 0
    (lambda (a s1)
      (k (cons 'true a)
         s1))))

(define (create-false s sr r k)
  (allocate s sr r 0
    (lambda (a s1)
      (k (cons 'false a)
         s1))))

(define (create-abstraction v s sr r k)
  (allocate s sr r 1
    (lambda (a s1)
      (k (cons 'abstraction a)
         (set s1 a v)))))





(define (mfxop fxop)
  (lambda (a* s k)
    (if (for-all (lambda (a)
                   (eq? 'number (car a)))
                 a*)
        (allocate-value
          (apply fxop (map (lambda (a)
                             (get s (cdr a)))
                           a*))
          s #f #f
          (lambda (a s1)
            (k a s1)))
        (error fxop
               "arguments have to be numbers"
               a*))))

(define (mcheck type)
  (lambda (a* s sr k)
    (if (= 1 (length a*))
        (allocate-value
          (eq? type (car (car a*)))
          s #f #f k)
        (error type
               "incorrect arity"
               a*))))


(define m+ (mfxop +))
(define m- (mfxop -))
(define m* (mfxop *))
(define m/ (mfxop /))
(define m= (mfxop =))
(define m< (mfxop <))
(define m> (mfxop >))
(define mnumber? (mcheck 'number))


(define mpair? (mcheck 'pair))

(define (mcons a* s k)
  (if (= 2 (length a*))
      (create-pair (car a*)
                   (cadr a*)
                   s #f #f k)
      (error 'cons
             "incorrect arity"
             a*)))

(define (mcar a* s k)
  (cond
    [(not (= 1 (length a*)))
     (error 'car "incorrect arity" a*)]
    [(not (eq? 'pair (car (car a*))))
     (error 'car "not a pair" (car a*))]
    [else
     (k (get s (cdr (car a*))) s)]))

(define (mcdr a* s k)
  (cond
    [(not (= 1 (length a*)))
     (error 'cdr "incorrect arity" a*)]
    [(not (eq? 'pair (car (car a*))))
     (error 'cdr "not a pair" (car a*))]
    [else
     (k (get s (+ 1 (cdr (car a*)))) s)]))


(define mnull? (mcheck 'null))



(define (mdebug a* s k)
  (display "Arguments:")
  (for-each (lambda (arg)
              (display " ")
              (write arg))
            a*)
  (newline)
  (display "Memory:") (newline)
  (display "  Size: ") (write (car s)) (newline)
  (display "  Contents:") (newline)
  (for-each (lambda (mem)
              (display "    ")
              (write mem)
              (newline))
            (cdr s))
  (create-number (car s) s #f #f k))



(define (create-store defs q)
  (if (null? defs)
      (q '() '() (cons 0 '()))
      (create-store (cddr defs)
        (lambda (n* a* s)
          (allocate-value (cadr defs)
                          s #f #f
                          (lambda (a s1)
                            (q (cons (car defs) n*)
                               (cons a a*)
                               s1)))))))


(define s.init #f)
(define r.global #f)
(define sr.global #f)


;#;
(create-store
  `(+       ,m+
    -       ,m-
    *       ,m*
    /       ,m/
    =       ,m=
    >       ,m>
    <       ,m<
    number? ,mnumber?
    pair?   ,mpair?
    cons    ,mcons
    car     ,mcar
    cdr     ,mcdr
    debug   ,mdebug
    )
  (lambda (n* a* s)
    (allocate-frame s '() '() a*
      (lambda (sr s1)
        (set! sr.global sr)
        (set! r.global (extend '() n*))
        (set! s.init s1)))))


(define (transcribe a s)
  (case (car a)
    [(number) (get s (cdr a))]
    [(null) '()]
    [(pair) (cons (transcribe (get s (cdr a)) s)
                  (transcribe (get s (+ 1 (cdr a))) s))]
    [(abstraction) (get s (cdr a))]
    [else (error 'transcribe
                 "invalid transcription input"
                 a)]))


(define (definition name body s sr r k)
  (if (pair? name)
      (definition (car name)
                  `((lambda ,(cdr name) ,@body))
                  s sr r k)
      (let* ([rr (extend r (list name))]
             [m (meaning (car body) rr)])
        (allocate-frame s sr rr '(0)
          (lambda (sr1 s1)
            (m s1 sr1
              (lambda (a s2)
                (k (set s2 (cdr sr1) a)
                   sr1
                   rr))))))))


(define (toplevel)
  (let loop ([s s.init]
             [sr sr.global]
             [r r.global])
    (display ">> ")
    (let ([in (read)])
      (if (and (pair? in)
               (eq? 'define (car in)))
          (definition (cadr in) (cddr in) s sr r loop)
          (let ([m (meaning in r)])
            (m s sr
              (lambda (a ss)
                (write (transcribe a ss))
                (newline)
                (loop ss sr r))))))))
