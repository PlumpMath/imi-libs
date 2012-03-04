(import (imi list processing)
        (imi string format)
        (imi sugar cut)
        (imi sugar receive)
        )


(define (fold-right* proc ls . seeds)
  (if (null? ls)
      (apply values seeds)
      (receive seeds (apply fold-right* proc (cdr ls) seeds)
        (apply proc (car ls) seeds))))




(define SEQUENCE 0)
(define RET      1)
(define WITHCONT 2)

(define (compile e r pos)
  (if (atom? e)
      (if (symbol? e) (compile-reference e r pos)
                      (compile-quotation e r pos))
      (case (car e)
        [(quote)      (compile-quotation (cadr e) r pos)]
        [(begin)      (compile-sequence (cdr e) r pos)]
        [(if)         (compile-alternative (cadr e) (caddr e) (cadddr e) r pos)]
        [(set!)       (compile-set (cadr e) (caddr e) r pos)]
        [(let)        (compile-let (cadr e) (cddr e) r pos)]
        [(lambda)     (compile-abstraction (cadr e) (cddr e) r pos)]
        [(define)     (compile-definition (cadr e) (cddr e) r pos)]
        [else         (compile-application (car e) (cdr e) r pos)])))


(define (atom? e) (not (pair? e)))


(define (compile-reference n r pos)
  (let* ([bind (lookup-variable r n)]
         [var (extract-variable bind)])
    (cond
      [(not bind)
       (error 'compile-reference
              "unbound variable"
              n r)]
      [(local? bind)
       (values (RETURN pos (car var))
               '()
               '())]
      [(and (global? var)
            (not (local? bind)))
       (values (RETURN pos (car var))
               (if (function? var)
                   '()
                   (list n))
               '())]
      [else
       (values (RETURN pos (car var))
               '()
               (add-free-ref n bind '()))])))


(define (compile-quotation e r pos)
  (if (atom? e)
      (values
        (cond
          [(char? e)     (RETURN pos (string #\" e #\"))]
          [(string? e)   (RETURN pos (string-append "'" e "'"))]
          [(symbol? e)   (RETURN pos (string-append "\"" (string-process-specials
                                                           (symbol->string e)) "\""))]
          [(number? e)   (RETURN pos (number->string e))]
          [(boolean? e)  (RETURN pos (if e "true" "false"))]
          [(null? e)     (RETURN pos "NULL")]
          [else          (error 'compile-quotation
                                "invalid quotation"
                                e)])
        '()
        '())
      (compile-list-quotation e r pos)))


(define (compile-list-quotation e* r pos)
  (receive (c* global* free*) (map-compile compile-quotation e* r)
    (values (list "array " (ARGUMENTS c*))
            global*
            free*)))



(define (compile-sequence e* r pos)
  (when (= pos WITHCONT)
    (error 'compile-sequence "a sequence can not be an argument" e*))
  (let ([e* (flatten-begin e*)])
    (receive (c* global* free*) (compile-sequence-list e* r pos)
      (values (list 'lines "{" (cons* 'indent 'lines c*) "}")
              global*
              free*))))

(define (compile-sequence-list e* r pos)
  (fold-right* (lambda (e c* global* free*)
                 (receive (c g* f*)
                          (compile e r
                            (if (null? c*) ; do we have the last element?
                                pos
                                SEQUENCE))
                   (values (cons (list c ";") c*)
                           (append g* global*)
                           (append f* free*))))
               e* '() '() '()))

(define (flatten-begin e*)
  (fold-right (lambda (e e*)
                (if (and (pair? e)
                         (eq? 'begin (car e)))
                    (append (flatten-begin (cdr e)) e*)
                    (cons e e*)))
              '()
              e*))



(define (compile-alternative ec et ef r pos)
  (if (= pos WITHCONT)
      (let-values ([(cc g0* f0*) (compile ec r WITHCONT)]
                   [(ct g1* f1*) (compile et r WITHCONT)]
                   [(cf g2* f2*) (compile ef r WITHCONT)])
        (values (list "(" cc " ? " ct " : " cf ")")
                (append g2* g1* g0*)
                (append f2* f1* f0*)))
      (let-values ([(cc g0* f0*) (compile ec r WITHCONT)]
                   [(ct g1* f1*) (compile-sequence (list et) r pos)]
                   [(cf g2* f2*) (compile-sequence (list ef) r pos)])
        (values (list 'lines
                      (list "if (" cc ")")
                      ct
                      "else"
                      cf)
                (append g2* g1* g0*)
                (append f2* f1* f0*)))))


(define (compile-set n e r pos)
  (receive (ce global* free*) (compile e r WITHCONT)
    (let ([bind (or (lookup-variable r n)
                    (error 'compile-set
                           "unbound variable"
                           n))])
      (values (SET (car (extract-variable bind))
                   ce)
              (if (and (global? bind)
                       (not (local? bind)))
                  (cons n global*)
                  global*)
              (if (and (not (global? bind))
                       (not (local? bind)))
                  (add-free-set n local free*)
                  free*)))))



(define (compile-let b* e* r pos)
  (cond
    [(= pos WITHCONT)
     (error 'compile-let
            "let cannot be with continuation (yet)"
            b* e*)]
    [(list? b*)
     (compile-simple-let b* e* r pos)]
    [else
     (compile-advanced-let b* (car e*) (cdr e*) r pos)]))

(define (compile-simple-let b* e* r pos)
  (let* ([n* (map car b*)]
         [v* (map cadr b*)]
         [r.local (extend-local* r n*)])
    (receive (c* global* free*) (compile-sequence-list e* r.local pos)
      (receive (ca global0* free0*) (compile-let-assignment n* v* r r.local)
        (values (cons* 'lines ca c*)
                (append global0* global*)
                (append free0*
                        (remp (lambda (f)
                                (memq (cadr f) n*))
                              free*)))))))

(define (compile-advanced-let n b* e* r pos)
  (error 'compile-advanced-let
         "not yet implemented"))


(define (compile-let-assignment n* e* r r.local)
  (receive (c* global* free*) (map-compile compile e* r)
    (values (list (if (null? (cdr n*))
                      (list (lookup-variable r.local (car n*))
                            " = "
                            (car c*))
                      (list "list ("
                            (list-intersperse ", "
                              (map (lambda (n)
                                     (lookup-local r.local n))
                                   n*))
                            ") = array ("
                            (list-intersperse ", " c*)
                            ")"))
                  ";")
            global*
            free*)))

                 

(define (compile-abstraction n* e* r pos)
  (receive (c global* free*) (compile-abstraction-root n* e* r)
    (values (FUNCTION c pos)
            global*
            free*)))



(define (compile-definition n* e* r pos)
  (if (list? n*) (compile-function-definition (car n*) (cdr n*) e* r pos)
                 (compile-variable-definition n* (car e*) r pos)))

(define (compile-function-definition n n* e* r pos)
  (if (not (= pos SEQUENCE))
      (error 'compile-function-definition
             "definition can only be in a sequence"
             n n* e* pos)
      (receive (c global* free*)
               (compile-abstraction-root n* e* (extend-local r (funbind n)))
        (values (DEFFUNCTION n c)
                global*
                free*))))


(define (compile-abstraction-root n* e* r)
  (receive (vars rest) (parse-vars n*)
    (receive (c* global* free*)
             (let* ([e* (flatten-begin e*)]
                    [r.func (extend-frame r (if rest
                                                (cons rest n*)
                                                n*))]
                    [r.body (analyse/env e* r.func)])
               (compile-sequence-list e* r.body RET))
      (values (ABSTRACTION vars rest c* free* global*)
              '()
              (map (lambda (f)
                     (list (car f)
                           (cadr f)
                           (car (caddr f))))
                   (remp (lambda (f)
                           (local? (car (caddr f))))
                         free*))))))



(define (compile-variable-definition n e r pos)
  (receive (c global* free*)
           (compile-set n e (extend-local r (varbind n r)) pos)
    (values (list c ";")
            global*
            free*)))



(define (compile-application e e* r pos)
  (let* ([bind (lookup-application r e)]
         [var (extract-variable bind)])
    (if (and var (procedure? (car var)))
        ((car var) e* r pos)
        (receive (c* global* free*) (map-compile compile e* r)
          (values (APPLICATION (or var (list (symbol->function e) 'global 'function))
                               c* pos)
                  (if (and var
                           (global? var)
                           (not (local? var)))
                      (cons e global*)
                      global*)
                  (if (and var
                           (not (global? var))
                           (not (local? var)))
                      (add-free-ref e bind free*)
                      free*))))))




(define (APPLICATION bind c* pos)
  (RETURN pos
    (list (car bind) " " (ARGUMENTS c*))))

(define (FUNCTION c pos)
  (RETURN pos
    (cons "function " c)))

(define (DEFFUNCTION n c)
  (cons* "function "
         (symbol->function n)
         " "
         c))

(define (ABSTRACTION vars rest c* free* global*)
  (list (ARGUMENTS (map symbol->variable vars))
        (collect-closure free*)
        " "
        (with-globals global*
          (if rest
              (cons (list (symbol->variable rest)
                          " = array_slice (func_get_vars (), "
                          (length vars)
                          ")")
                    c*)
              c*))))

(define (SET var val pos)
  (let ([c (list var " = " val)])
    (if (= pos WITHCONT)
        (parantheses c)
        (RETURN pos c))))

(define (ARGUMENTS als)
  (parantheses
    (list-intersperse ", " als)))

(define (RETURN pos c)
  (if (= pos RET) (list "return " c)
                     c))





(define (analyse/env e* r)
  (cond
    [(null? e*) r]
    [(pair? e*)
     (case (car e*)
       [(define)
        (analyse/env (cdr e*)
          (extend-local r
            ((if (list? (cadr e*)) funbind
                                   varbind)
             (car e*))))]
       [else
        (analyse/env (cdr e*) r)])]
    [else
     (analyse/env (cdr e*) r)]))

(define (parse-vars n*)
  (let loop ([vars '()]
             [n* n*])
    (cond
      [(null? n*)
       (values (reverse vars) #f)]
      [(pair? n*)
       (loop (cons (car n*) vars)
             (cdr n*))]
      [else
       (values (reverse vars) n*)])))


(define (collect-closure free*)
  (if (null? free*)
      ""
      (list " use "
        (parantheses
          (list-intersperse ", "
            (map (lambda (f)
                   (case (car f)
                     [(set) (string-append "&" (car (caddr f)))]
                     [(ref) (car (caddr f))]
                     [else  (error 'collect-closure
                                   "invalid free variable"
                                   f)]))
                 (merge-free free*)))))))


(define (merge-free free*)
  (fold-left (lambda (merged free)
               (let ([thisfree? (lambda (f)
                                  (eq? (cadr f)
                                       (cadr free)))])
                 (cond
                   [(find thisfree? merged)
                    => (lambda (f)
                         (cond
                           [(eq? 'set (car f)) merged]
                           [(eq? 'set (car free))
                            (cons free (remp thisfree? merged))]
                           [else merged]))]
                   [else (cons free merged)])))
             '()
             free*))


(define (with-globals global* c*)
  (let ([globals (list-merge-unique equal? global*)])
    (list 'lines
          "{"
          (cons* 'indent 'lines
            (if (null? globals)
                c*
                (cons (list "global "
                            (list-intersperse ", "
                              (map cdr globals))
                            ";")
                      c*)))
          "}")))


(define (map-compile comp e* r)
  (fold-right* (lambda (e c* global* free*)
                 (receive (c g* f*) (comp e r WITHCONT)
                   (values (cons c c*)
                           (append g* global*)
                           (append f* free*))))
               e* '() '() '()))

(define (parantheses ls)
  (append '("(") ls '(")")))







(define (symbol->variable sym)
  (string-append "$" (string-process-specials
                       (symbol->string sym))))

(define (symbol->function sym)
  (string-process-specials
    (symbol->string sym)))


(define (string-process-specials str)
  (list->string
    (fold-right (lambda (c ls)
                  (let ([special (assq c *special-table*)])
                    (if special (append (string->list (cadr special))
                                        ls)
                                (cons c ls))))
                '()
                (string->list str))))

(define *special-table*
  '((#\+   "_plus_")
    (#\-   "_")
    (#\*   "_times_")
    (#\/   "_div_")
    (#\%   "_mod_")
    (#\$   "_")
    (#\?   "_is")
    ))




(define (varbind n r)
  (cond
    [(lookup-variable r n)
     => (lambda (bind)
          `(,n (variable
                 ,(shadow-variable
                    (car (extract-variable bind))))))]
    [else
     `(,n (variable ,(symbol->variable n)))]))

(define (funbind n)
  `(,n (function ,(symbol->function n))
       (variable ,(asstring n))))


(define (shadow-variable varstr)
  (string-append "$_" (list->string
                        (cdr (string->list varstr)))))

(define (asstring n)
  (string-append "'" (symbol->string n) "'"))




(define (extend-local r bind)
  (cons (car r)
        (cons (if (null? (car r))
                  (cons* (car bind)
                         '(global)
                         (cdr bind))
                  bind)
              (cdr r))))

(define (extend-local* r bind*)
  (let loop ([r.new r]
             [bind* bind*])
    (if (null? bind*)
        r.new
        (loop (extend-local r.new (varbind (car bind*) r))
              (cdr bind*)))))

(define (extend-frame r n*)
  (cons r (map (cut varbind <> '()) n*)))




(define (lookup r n proc)
  (cond
    [(null? r) #f]
    [(assq n (cdr r))
     => (lambda (ref)
          (proc (cdr ref)))]
    [(lookup (car r) n proc)
     => list]
    [else #f]))

(define (lookup-variable r n)
  (lookup r n
    (lambda (ref)
      (cond
        [(assq 'variable ref)
         => (lambda (var)
              (process-special (cadr var) ref))]
        [else (error 'lookup-variable
                     "no mode for variable defined"
                     ref n r)]))))

(define (lookup-application r n)
  (lookup r n
    (lambda (ref)
      (cond
        [(assq 'function ref)
         => (lambda (func)
              (process-special (cadr func) ref))]
        [(assq 'variable ref)
         => (lambda (var)
              (process-special (cadr var) ref))]
        [else (error 'lookup-application
                     "no mode for application defined"
                     ref n r)]))))



(define (process-special var special)
  (cons var
        (filter (lambda (s) (assq s special))
                '(global function))))

(define (extract-variable bind)
  (if (or (not bind)
          (local? bind))
      bind
      (extract-variable (car bind))))



(define (global-env? r)
  (null? (car r)))

(define (local? bind)
  (not (list? (car bind))))

(define (global? var)
  (memq 'global (cdr var)))

(define (function? var)
  (memq 'function (cdr var)))



(define (add-free-ref n bind free*)
  (cons (list 'ref n bind)
        free*))

(define (add-free-set n bind free*)
  (cons (list 'set n bind)
        free*))





(define gensym
  (let ([counter 0])
    (lambda (sym)
      (set! counter (+ 1 counter))
      (string->symbol
        (string-append (symbol->string sym)
                       "-gensym-"
                       (number->string counter))))))





(define (check-args who args len)
  (unless (= len (length args))
    (error who
           "invalid number of arguments"
           args)))


(define php-fold
  "function ($func, $init, $arr) { return array_reduce ($ls, $proc, $init); }")
(define (php-fold-proc args r pos)
  (check-args 'fold args 3)
  (let ([proc (car args)]
        [init (cadr args)]
        [ls (caddr args)])
    (receive (c* global* free*) (map-compile compile (list ls proc init) r)
      (values (RETURN pos
                (list "array_reduce ("
                      (list-intersperse ", " c*)
                      ")"))
              global*
              free*))))


(define php-for-each
  "function ($func, $arr) { foreach ($arr as $v) { $func ($arr); } }")
(define (php-for-each-proc args r pos)
  (unless (= pos SEQUENCE)
    (error 'for-each "can only be in sequence" args pos))
  (check-args 'for-each args 2)
  (let ([proc (car args)]
        [ls (cadr args)])
    (cond
      [(not (symbol? ls))
       (let ([lsid (gensym 'list)])
         (compile `(let ([,lsid ,ls]) (for-each ,proc ,lsid)) r pos))]
      [(and (pair? proc)
            (eq? 'lambda (car proc)))
       (let ([largs (cadr proc)]
             [lbody (cddr proc)])
         (unless (= 1 (length largs))
           (error 'for-each "proc's arity must be 1" proc))
         (let* ([newvar (varbind (car largs) r)]
                [r.body (extend-local r newvar)])
           (let-values ([(c g0* f0*) (compile ls r WITHCONT)]
                        [(c* g1* f1*) (compile-sequence lbody r.body pos)])
             (values (list "foreach " (parantheses (list c " as " (car (lookup-variable r.body (car largs)))))
                           c*)
                     (append g1* g0*)
                     (append f1* f0*)))))]
      [(not (symbol? proc))
       (let ([procid (gensym 'func)])
         (compile `(let ([,procid ,proc]) (for-each ,procid ,ls)) r pos))]
      [else
       (let ([valueid (gensym 'v)])
         (values (list "foreach " (parantheses (symbol->variable ls)
                                               " as " 
                                               (symbol->variable valueid)) " "
                       (list 'lines
                             "{"
                             (list 'indent (symbol->variable proc) " (" valueid ");")
                             "}"))
                 '()
                 '()))])))



(define php-list "function () { return func_get_args(); }")


(define php-cons "function ($elem, $arr) { return array_unshift ($arr, $elem); }")
(define (php-cons-proc e* r pos)
  (check-args 'cons e* 2)
  (receive (c* global* free*) (map-compile compile e* r)
    (values (RETURN pos
              (list "array_unshift "
                    (ARGUMENTS (list (cadr c*)
                                     (car c*)))))
            global*
            free*)))



(define php-car "function ($arr) { return $arr[0]; }")
(define (php-car-proc e* r pos)
  (check-args 'car e* 1)
  (receive (c global* free*) (compile (car e*) r WITHCONT)
    (values (RETURN pos (list c "[0]"))
            global*
            free*)))


(define php-cdr "function ($arr) { return array_slice ($arr, 1); }")
(define (php-cdr-proc e* r pos)
  (check-args 'cdr e* 1)
  (receive (c global* free*) (compile (car e*) r WITHCONT)
    (values (RETURN pos
              (list "array_slice "
                    (ARGUMENTS (list c "1"))))
            global*
            free*)))


(define (operator op)
  (lambda (e* r pos)
    (receive (c* global* free*) (map-compile compile e* r)
      (values (RETURN pos
                (parantheses
                  (list-intersperse
                    (string-append " " op " ")
                    c*)))
              global*
              free*))))

(define (comparator cmp)
  (lambda (e* r pos)
    (receive (c* global* free*) (map-compile compile e* r)
      (values (RETURN pos
                (parantheses
                  (list-intersperse
                    " and "
                    (let ([strcomp (string-append " " cmp " ")])
                      (intermap 2
                        (lambda (a b)
                          (list a strcomp b))
                        c*)))))
              global*
              free*))))



(define *r.global*
  `(()
    (list     (global) (function "array") (variable ,php-list))
    (cons     (global) (function ,php-cons-proc) (variable ,php-cons))
    (car      (global) (function ,php-car-proc)  (variable ,php-car))
    (cdr      (global) (function ,php-cdr-proc)  (variable ,php-cdr))
    (length   (global) (function "count"))
              
    (map      (global) (function "array_map"))
    (for-each (global) (function ,php-for-each-proc) (variable ,php-for-each))
    (filter   (global) (function "array_filter"))
    (reverse  (global) (function "array_reverse"))
    (fold     (global) (function ,php-fold-proc) (variable ,php-fold))
              
    (echo     (global) (function "echo"))
              
    (+        (global) (function ,(operator "+")))
    (-        (global) (function ,(operator "-")))
    (*        (global) (function ,(operator "*")))
    (/        (global) (function ,(operator "/")))
    (mod      (global) (function ,(operator "%")))
    (eq?      (global) (function ,(comparator "===")))
    (=        (global) (function ,(comparator "==")))
    (<        (global) (function ,(comparator "<")))
    (>        (global) (function ,(comparator ">")))
    (<=       (global) (function ,(comparator "<=")))
    (>=       (global) (function ,(comparator ">=")))
              
    (and      (global) (function ,(operator "and")))
    (or       (global) (function ,(operator "or")))

    (string-append   (global) (function ,(operator ".")))
    ))


(define (indent n)
  (display (make-string (* 2 n) #\space)))

(define (write-compilation c ind)
  (cond
    [(list? c)
     (case (and (pair? c) (car c))
       [(lines)
        (unless (null? (cdr c))
          (write-compilation (cadr c) ind)
          (for-each (lambda (c)
                      (newline)
                      (indent ind)
                      (write-compilation c ind))
                    (cddr c)))]
       [(indent)
        (indent 1)
        (write-compilation (cdr c) (+ 1 ind))]
       [else
        (for-each (cut write-compilation <> ind) c)])]
    [(string? c)
     (display c)]
    [else (error 'write-compilation
                 "can't write compilation"
                 c)]))


(define (warn-free free*)
  (define warning    "Warning: free identifiers: ")
  (define whitespace "                           ")
  (unless (null? free*)
    (display warning)
    (display (car free*))
    (newline)
    (for-each (lambda (free)
                (display whitespace)
                (display free)
                (newline))
              (cdr free*))
    (newline)))

(define (scomp e)
  (receive (c global* free*) (compile e *r.global* SEQUENCE)
    (warn-free free*)
    (write-compilation c 0)
    (newline)))
