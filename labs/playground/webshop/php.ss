(library (imi labs playground webshop php)
  (export position position-set

          compile
          compile-quotation
          compile-quasiquote
          compile-reference
          compile-set
          compile-sequence
          compile-alternative
          compile-let
          compile-abstraction
          compile-definition
          compile-class
          compile-make
          compile-send
          compile-static-send
          compile-slot
          compile-static-slot
          compile-instanceof
          compile-application

          write-c
          c-width
          width-fits?
          maxwidth
          indentwidth

          analyse-body/env
          analyse
          analyse-sequence
          analyse-definition

          collect-free
          collect-free-quotation
          collect-free-quasiquote
          collect-free-reference
          collect-free-set
          collect-free-sequence
          collect-free-alternative
          collect-free-let
          collect-free-abstraction
          collect-free-definition
          collect-free-class
          collect-free-send
          collect-free-static-send
          collect-free-slot
          collect-free-static-slot
          collect-free-instanceof
          collect-free-application

          local?
          free?
          global?
          remove-one-frame

          varbind
          funbind
          gensymbind
          specialbind
          extend-frame
          extend-local
          extend-local*
          extend-local**
          lookup
          lookup-variable
          lookup-function
          lookup-type

          gensym
          symbol->variable
          symbol->function
          symbol->class
          shadow-variable
          shadow-function
          process-string

          map-compile
          map-compile*

          POSHANDLER
          ARGLIST
          CLOSURE
          GLOBALS

          binaryop
          operator
          comparator
          keywordop
          r.global
          scomp)
  (import (rnrs)
          (imi list utils)
          (imi list processing)
          (imi io string)
          (imi sugar cut)
          (imi sugar receive)
          (imi utils parameter))


  (define-enumeration position
    (return argument sequence)
    position-set)



  (define (compile e r pos)
    (if (atom? e)
        (if (symbol? e) (compile-reference e r pos)
                        (compile-quotation e r pos))
        (case (car e)
          [(quote)      (compile-quotation (cadr e) r pos)]
          [(quasiquote) (compile-quasiquote (cadr e) r pos)]
          [(set!)       (compile-set (cadr e) (caddr e) r pos)]
          [(if)         (compile-alternative (cadr e) (caddr e) (cadddr e) r pos)]
          [(begin)      (compile-sequence (cdr e) r pos)]
          [(let)        (compile-let (cadr e) (cddr e) r pos)]
          [(lambda)     (compile-abstraction (cadr e) (cddr e) r pos)]
          [(define)     (compile-definition (cadr e) (cddr e) r pos)]
          [(define-class) (compile-class (cadr e) (cddr e) r pos)]
          [(make)       (compile-make (cadr e) (cddr e) r pos)]
          [(send)       (compile-send (cadr e) (caddr e) (cdddr e) r pos)]
          [($send)      (compile-static-send (cadr e) (caddr e) (cdddr e) r pos)]
          [(slot)       (compile-slot (cadr e) (caddr e) r pos)]
          [($slot)      (compile-static-slot (cadr e) (caddr e) r pos)]
          [(instanceof) (compile-instanceof (cadr e) (caddr e) r pos)]
          [else         (compile-application (car e) (cdr e) r pos)])))


  (define (atom? e) (not (pair? e)))





  (define (compile-reference n r pos)
    (values '()
      (POSHANDLER pos
        (or (lookup-variable r n)
            (error 'compile-reference
                   "unbound variable"
                   n)))))




  (define (compile-quotation e r pos)
    (if (atom? e)
        (values '()
          (POSHANDLER pos
            (cond
              [(char? e) (string #\" e #\")]
              [(string? e) (with-output-to-string (lambda () (write e)))]
              [(symbol? e) (string-append "'" (process-string (symbol->string e)) "'")]
              [(number? e) (number->string e)]
              [(boolean? e) (if e "TRUE" "FALSE")]
              [(null? e) "NULL"]
              [else (error 'compile-quotation
                           "cannot quote data"
                           e)])))
        (cond
          [(list? e)
           (receive (pre* c*) (map-compile compile-quotation e r (position argument))
             (values pre*
               (POSHANDLER pos
                 (list "array (" (ARGLIST c*) ")"))))]
          [else (error 'compile-quotation
                       "cannot quote data"
                       e)])))




  (define (compile-quasiquote e r pos)
    (if (atom? e)
        (compile-quotation e r pos)
        (cond
          [(list? e)
           (case (car e)
             [(unquote) (compile (cadr e) r (position argument))]
             [else
              (receive (pre* c*) (map-compile compile-quasiquote e r (position argument))
                (values pre*
                  (POSHANDLER pos
                    (list "array (" (ARGLIST c*) ")"))))])]
          [else (error 'compile-quasiquote
                       "cannot quote data"
                       e)])))




  (define (compile-set n e r pos)
    (let-values ([(pre.e c.e) (compile e r (position argument))]
                 [(pre.n c.n) (compile-set-ref n r)])
      (let ([cset (list c.n " = " c.e)])
        (values (append pre.n pre.e)
          (POSHANDLER pos
            (if (enum-set-member? pos (position-set return argument))
                (parantheses cset)
                cset))))))

  (define (compile-set-ref n r)
    (cond
      [(symbol? n)
       (values '()
         (or (lookup-variable r n)
             (error 'compile-set "unbound variable" n)))]
      [(and (pair? n)
            (memq (car n) '(slot $slot)))
       (compile n r (position argument))]
      [else
       (error 'compile-set
              "cannot set this .. oO"
              n)]))




  (define (compile-alternative ec et ef r pos)
    (let-values ([(prec cc) (compile ec r (position argument))]
                 [(pret ct) (compile et r pos)]
                 [(pref cf) (compile ef r pos)])
      (cond
        [(enum-set-member? pos (position-set return sequence))
         (values prec
           (IFSTATEMENT cc
             (append pret (list ct))
             (append pref (list cf))))]
        [(and (not (null? pret))
              (not (null? pref)))
         (let ([tmpvar (symbol->variable (gensym 'condition))])
           (values (append prec
                           (list tmpvar " = " cc "; ")
                           (IFSTATEMENT tmpvar pret pref))
             (POSHANDLER pos
               (parantheses (list tmpvar " ? " ct " : " cf)))))]
        [else
         (values (append prec pret pref)
           (POSHANDLER pos
             (parantheses (list cc " ? " ct " : " cf))))])))




  (define (compile-sequence e* r pos)
    (receive (pre c) (split-last (map-compile* compile e* r (position sequence) pos))
      (values pre
              c)))




  (define (compile-let b* e* r pos)
    (let ([n* (map car b*)]
          [eb* (map cadr b*)])
      (let* ([r.local (if (eq? pos (position return))
                          (extend-local* r n*)
                          (extend-local** r n*))]
             [r.body (analyse-body/env e* r.local)])
        (let-values ([(preb* cb*) (map-compile compile eb* r (position argument))]
                     [(pree* ce*) (compile-sequence e* r.body pos)])
          (values (append (list
                            (if (= 1 (length b*))
                                (list (lookup-variable r.body (car n*))
                                      " = "
                                      (car cb*)
                                      "; ")
                                (list "list ("
                                      (ARGLIST (map (lambda (n)
                                                      (lookup-variable r.body n))
                                                    n*))
                                      ") = array ("
                                      (ARGLIST cb*)
                                      "); ")))
                          preb*
                          pree*)
                  ce*)))))




  (define (compile-abstraction n* e* r pos)
    (let* ([r.func (extend-frame r n*)]
           [r.body (analyse-body/env e* r.func)]
           [free* (collect-free-sequence e* r.body)])
      (values '()
        (POSHANDLER pos
          (list 'group
                (list "function ("
                      (ARGLIST (map symbol->variable n*))
                      ") "
                      (CLOSURE free*)
                      "{ ")
                (compile-body e* r.body free*)
                "}")))))



  (define (compile-definition var body r pos)
    (if (not (eq? pos (position sequence)))
        (error 'compile-definition
               "definition where an expression was expected"
               var body)
        (if (list? var)
            (compile-function-definition (car var) (cdr var) body r pos)
            (compile-variable-definition var (car body) r pos))))

  (define (compile-function-definition n n* e* r pos)
    (let* ([name (lookup-function r n)]
           [r.func (extend-frame r n*)]
           [r.body (analyse-body/env e* r.func)]
           [free* (collect-free-sequence e* r.body)])
      (if (exists free? free*)
          (compile-closure-definition n n* e* name r r.func r.body free* pos)
          (values '()
            (list 'group
                  (list "function " name " (" (ARGLIST (map symbol->variable n*)) ") { ")
                  (compile-body e* r.body free*)
                  "} ")))))

  (define (compile-closure-definition n n* e* name r r.func r.body free* pos)
    (let ([args (ARGLIST (map symbol->variable n*))]
          [closure (CLOSURE free*)])
      (values '()
        (list 'group
              (list name " = function (" args ") " closure "{ ")
              (compile-body e* r.body free*)
              "}; "))))


  (define (compile-variable-definition n e r pos)
    (receive (pre c) (compile e r (position argument))
      (values pre
        (list (lookup-variable r n)
              " = "
              (postattach c "; ")))))





  (define (compile-class def slots r pos)
    (unless (eq? pos (position sequence))
      (error 'compile-class
             "only allowed in a sequence"
             def slots))
    (let ([name (car def)]
          [extends (and (pair? (cdr def))
                        (cadr def))])
      (values '()
        (list 'group
              (list "class " (lookup-class r name)
                    (if extends
                        (list "extends " (lookup-class r extends))
                        (list))
                    " { ")
              (cons* 'indent 'group
                     (map (lambda (slot)
                            (compile-class-slot slot r))
                          slots))
              "} "))))

  (define (compile-class-slot def r)
    (receive (type var body) (analyse-class-slot def)
      (if (list? var)
          (compile-class-function type (car var) (cdr var) body r)
          (compile-class-variable type var (and (pair? body) (car body)) r))))


  (define (analyse-class-slot def)
    (let ([accessibility (car def)])
      (unless (memq accessibility '(private protected public))
        (error 'compile-class-slot
               "invalid accessibility, has to be private, protected or public"
               accessibility))
      (if (eq? 'static (cadr def))
          (values (string-append (symbol->string accessibility) " static")
                  (caddr def)
                  (cdddr def))
          (values (symbol->string accessibility)
                  (cadr def)
                  (cddr def)))))


  (define (compile-class-function type n n* e* r)
    (let* ([r.func (extend-frame r n*)]
           [r.obj (extend-local r.func '(this (variable "$this")))]
           [r.class (extend-local r.obj '(parent (class "parent")))]
           [r.body (analyse-body/env e* r.class)]
           [free* (collect-free-sequence e* r.body)])
      (if (exists free? free*)
          (error 'compile-class-function
                 "cannot close variables in a method"
                 (filter free? free*))
          (list 'group
                (list type " function " (symbol->function n)
                      " (" (ARGLIST (map symbol->variable n*)) ") { ")
                (compile-body e* r.body free*)
                "} "))))

  (define (compile-class-variable type n e r)
    (if e
        (receive (pre* c) (compile e r (position argument))
          (unless (null? pre*)
            (error 'compile-class-variable
                   "can only have simple class variable value expressions"
                   n e))
          (list type " " (symbol->variable n) " = " c "; "))
        (list type " " (symbol->variable n) "; ")))


  (define (compile-make class e* r pos)
    (let-values ([(pre c.class) (if (symbol? class)
                                    (values '() (lookup-class r class))
                                    (compile class r (position argument)))]
                 [(pre* c*) (map-compile compile e* r (position argument))])
      (values (append pre pre*)
        (POSHANDLER pos
          (list "new " c.class " (" (ARGLIST c*) ")")))))


  (define (compile-send obj meth e* r pos)
    (receive (pre* c*) (map-compile compile e* r (position argument))
      (let ([send-end (list "->" (symbol->function meth) " (" (ARGLIST c*) ")")])
        (if (symbol? obj)
            (values pre*
              (POSHANDLER pos
                (cons (lookup-variable r obj) send-end)))
            (let ([tmpobj (gensym 'obj)])
              (receive (pre c) (compile obj r (position argument))
                (values (append pre* pre
                          (list (list (symbol->variable tmpobj) " = " c)))
                        (POSHANDLER pos
                          (cons (symbol->variable tmpobj) send-end)))))))))

  (define (compile-static-send class meth e* r pos)
    (let-values ([(pre c.class) (if (symbol? class)
                                    (values '() (lookup-class r class))
                                    (compile class r (position argument)))]
                 [(pre* c*) (map-compile compile e* r (position argument))])
      (values (append pre pre*)
        (POSHANDLER pos
          (list c.class "::" (symbol->function meth)
                " (" (ARGLIST c*) ")")))))


  (define (compile-slot obj slot r pos)
    (receive (pre.obj c.obj) (compile obj r (position argument))
      (values pre.obj
        (POSHANDLER pos
          (list c.obj "->" (symbol->variable slot))))))


  (define (compile-static-slot class slot r pos)
    (receive (pre c.class) (if (symbol? class)
                               (values '() (lookup-class r class))
                               (compile class r (position argument)))
      (values pre
        (POSHANDLER pos
          (list c.class "::" (symbol->variable slot))))))


  (define (compile-instanceof e class r pos)
    (let-values ([(pre c) (compile e r (position argument))]
                 [(pre.class c.class) (if (symbol? class)
                                          (values '() (lookup-class r class))
                                          (compile class r (position argument)))])
      (values (append pre pre.class)
        (POSHANDLER pos
          (parantheses (list c " instanceof " c.class))))))





  (define (compile-application ef e* r pos)
    (receive (pre* c*) (map-compile compile e* r (position argument))
      (if (not (symbol? ef))
          (let ([tmpvar (symbol->variable (gensym 'func))])
            (receive (pref cf) (compile ef r (position argument))
              (values
                (append pref
                        (list (list tmpvar " = " cf "; "))
                        pre*)
                (POSHANDLER pos
                  (list (string-append tmpvar " (")
                        (ARGLIST c*)
                        ")")))))
          (let ([var (or (lookup-function r ef)
                         (error 'compile-application
                                "unbound variable"
                                ef))])
            (if (procedure? var)
                (var e* r pos)
                (values pre*
                  (POSHANDLER pos
                    (list (string-append var " (")
                          (ARGLIST c*)
                          ")"))))))))

  (define (compile-body e* r free*)
    (receive (pre* c*) (map-compile compile e* r (position return))
      (cons* 'indent 'group
        (append (GLOBALS free*)
                pre*
                c*))))






  (define (analyse-body/env e* r)
    (fold-left (lambda (r.body e)
                 (analyse e r.body))
               r
               e*))

  (define (analyse e r)
    (if (atom? e)
        r
        (case (car e)
          [(begin)  (analyse-sequence (cdr e) r)]
          [(define) (analyse-definition (cadr e) (cddr e) r)]
          [(define-class) (analyse-class (cadr e) (cddr e) r)]
          [else r])))

  (define (analyse-sequence e* r)
    (if (null? e*)
        r
        (analyse-sequence (cdr e*) (analyse (car e*) r))))

  (define (analyse-definition var body r)
    (if (list? var)
        (analyse-function-definition (car var) (cdr var) body r)
        (analyse-variable-definition var (car body) r)))

  (define (analyse-function-definition n n* e* r)
    (let* ([r.local (extend-local r (funbind n r))]
           [r.func (extend-frame r.local n*)]
           [r.body (analyse-body/env e* r.func)]
           [free* (collect-free-sequence e* r.body)])
      (if (exists free? free*)
          (analyse-closure-definition n n* e* r r.func r.body free*)
          r.local)))

  (define (analyse-closure-definition n n* e* r r.func r.body free*)
    (extend-local r (varbind n r)))

  (define (analyse-variable-definition n e r)
    (extend-local r (varbind n r)))


  (define (analyse-class def slots r)
    (extend-local r (if (global-env? r)
                        (classbind (car def) r)
                        (gensymbind classbind (car def) r))))




  (define (collect-free e r)
    (if (atom? e)
        (if (symbol? e) (collect-free-reference e r)
                        (collect-free-quotation e r))
        (case (car e)
          [(quote)      (collect-free-quotation (cadr e) r)]
          [(quasiquote) (collect-free-quasiquote (cadr e) r)]
          [(set!)       (collect-free-set (cadr e) (caddr e) r)]
          [(if)         (collect-free-alternative (cadr e) (caddr e) (cadddr e) r)]
          [(begin)      (collect-free-sequence (cdr e) r)]
          [(let)        (collect-free-let (cadr e) (cddr e) r)]
          [(lambda)     (collect-free-abstraction (cadr e) (cddr e) r)]
          [(define)     (collect-free-definition (cadr e) (cddr e) r)]
          [(define-class) (collect-free-class (cadr e) (cddr e) r)]
          [(make)       (collect-free-make (cadr e) (cddr e) r)]
          [(send)       (collect-free-send (cadr e) (caddr e) (cdddr e) r)]
          [($send)      (collect-free-static-send (cadr e) (caddr e) (cdddr e) r)]
          [(slot)       (collect-free-slot (cadr e) (caddr e) r)]
          [($slot)      (collect-free-static-slot (cadr e) (caddr e) r)]
          [(instanceof) (collect-free-instanceof (cadr e) (caddr e) r)]
          [else         (collect-free-application (car e) (cdr e) r)])))


  (define (collect-free-reference n r)
    (let ([var (lookup-variable r n)])
      (if var
          (let ([type (lookup-type r n)])
            (case type
              [(0) '()]
              [(class) '()]
              [(global) (list (cons type var))]
              [else (list (list type 'ref var n))]))
          (error 'collect-free-reference
                 "unbound variable"
                 n r))))

  (define (collect-free-quotation e r)
    '())

  (define (collect-free-quasiquote e r)
    (if (atom? e)
        (collect-free-quotation e r)
        (cond
          [(list? e)
           (case (car e)
             [(unquote) (collect-free (cadr e) r)]
             [else (map-collect-free collect-free-quasiquote e r)])]
          [else
           (error 'collect-free-quasiquote
                  "invalid quotation"
                  e)])))

  (define (collect-free-set n e r)
    (let ([free.e (collect-free e r)]
          [free.n (collect-free-set-ref n r)])
      (append free.e free.n)))

  (define (collect-free-set-ref n r)
    (cond
      [(symbol? n)
       (let ([var (lookup-variable r n)])
         (if var
             (let ([type (lookup-type r n)])
               (case type
                 [(0) '()]
                 [(class) '()]
                 [(global) (list (cons type var))]
                 [else (list (list type 'set var n))]))
             (error 'collect-free-set
                    "unbound variable"
                    n r)))]
      [(and (pair? n)
            (memq (car n) '(slot $slot)))
       (collect-free n r)]
      [else
       (error 'collect-free-set
              "invalid thing to be set .."
              n)]))


  (define (collect-free-alternative ec et ef r)
    (append (collect-free ec r)
            (collect-free et r)
            (collect-free ef r)))

  (define (collect-free-sequence e* r)
    (fold-left (lambda (free e)
                 (append (collect-free e r)
                         free))
               '()
               e*))

  (define (collect-free-let b* e* r)
    (let ([n* (map car b*)]
          [eb* (map cadr b*)])
      (let* ([r.local (extend-local* r n*)]
             [r.body (analyse-body/env e* r.local)])
        (append (map-collect-free collect-free eb* r)
                (collect-free-sequence e* r.body)))))

  (define (collect-free-abstraction n* e* r)
    (let* ([r.func (extend-frame r n*)]
           [r.body (analyse-body/env e* r.func)])
      (remp global?
            (remove-one-frame
              (collect-free-sequence e* r.body)))))

  (define (collect-free-definition var body r)
    (if (list? var)
        (let ([r.local (extend-local r (funbind (car var) r))])
          (collect-free-abstraction (cdr var) body r.local))
        (collect-free (car body) r)))

  (define (collect-free-class def slots r)
    '())

  (define (collect-free-make class e* r)
    (let ([class.free (collect-free class r)]
          [arg.free* (map-collect-free collect-free e* r)])
      (append class.free arg.free*)))

  (define (collect-free-send obj meth e* r)
    (let ([obj.free (collect-free obj r)]
          [arg.free* (map-collect-free collect-free e* r)])
      (append obj.free arg.free*)))

  (define (collect-free-static-send class meth e* r)
    (let ([class.free (collect-free class r)]
          [arg.free* (map-collect-free collect-free e* r)])
      (append class.free arg.free*)))

  (define (collect-free-slot obj slot r)
    (collect-free obj r))

  (define (collect-free-static-slot class slot r)
    (collect-free class r))

  (define (collect-free-instanceof e class r)
    (let ([e.free (collect-free e r)]
          [class.free (collect-free class r)])
      (append e.free class.free)))

  (define (collect-free-application ef e* r)
    (let ([func.free
            (if (symbol? ef)
                (let ([var (lookup-function r ef)])
                  (if var
                      (let ([type (lookup-type r ef)])
                        (case type
                          [(0 global) '()]
                          [else (list (list type 'ref var ef))]))
                      (error 'collect-free-application
                             "unbound variable"
                             ef r)))
                (collect-free ef r))])
      (append func.free (map-collect-free collect-free e* r))))


  (define (local? free)
    (eq? 0 (car free)))

  (define (free? free)
    (and (number? (car free))
         (not (zero? (car free)))))

  (define (global? free)
    (eq? 'global (car free)))


  (define (map-collect-free collect e* r)
    (fold-left (lambda (free* e)
                 (append (collect e r)
                         free*))
               '()
               e*))

  (define (remove-one-frame free*)
    (map (lambda (free)
           (if (global? free)
               free
               (cons (- (car free) 1)
                     (cdr free))))
         (remp local? free*)))
                





  (define (map-compile comp e* r pos)
    (let ([pos.body (case pos
                      [(sequence return) (position sequence)]
                      [(argument) (position argument)]
                      [else (error 'map-compile "invalid position" pos)])]
          [pos.end pos])
      (let loop ([pre* '()]
                 [c*rev '()]
                 [e* e*])
        (if (null? e*)
            (values pre* (reverse c*rev))
            (receive (pre c) (comp (car e*) r (if (null? (cdr e*)) pos.end
                                                                   pos.body))
              (loop (append pre* pre)
                    (cons c c*rev)
                    (cdr e*)))))))


  (define (map-compile* comp e* r pos.body pos.end)
    (let loop ([pre* '()]
               [e* e*])
      (if (null? e*)
          pre*
          (receive (pre c) (comp (car e*) r (if (null? (cdr e*)) pos.end
                                                                 pos.body))
            (loop (append pre*
                          pre
                          (list c))
                  (cdr e*))))))
           



  (define (split-last c*)
    (let ([c*rev (reverse c*)])
      (values (reverse (cdr c*rev))
              (car c*rev))))

  (define (preattach sth c)
    (if (list? c)
        (cons (preattach sth (car c))
              (cdr c))
        (string-append sth c)))

  (define (postattach c sth)
    (if (list? c)
        (receive (c* c) (split-last c)
          (append c* (list (postattach c sth))))
        (string-append c sth)))



  (define (parantheses c)
    (preattach "(" (postattach c ")")))

  (define gensym
    (let ([counter 0])
      (lambda (name)
        (set! counter (+ 1 counter))
        (string->symbol
          (string-append
            (symbol->string name)
            "-gensym-"
            (number->string counter))))))



  (define (IFSTATEMENT cc ct cf)
    (list 'group
          (list "if (" cc ") { ")
          (cons* 'indent 'group ct)
          "} else { "
          (cons* 'indent 'group cf)
          "}"))

  (define (GLOBAL-FUNCTION-DEFINITION n args e* free* r r.func)
    (list (list (string-append
                  "function "
                  (lookup-function r n)
                  " (")
                (ARGLIST args)
                ") { ")
          (compile-body e* r.func free*)
          "} "))

  (define (ARGLIST c*)
    (if (null? c*)
        '()
        (receive (c* c) (split-last c*)
          (cons 'elements
            (append (map (lambda (c)
                           (postattach c ", "))
                         c*)
                    (list c))))))

  (define (CLOSURE free*)
    (let ([local (filter free? free*)])
      (if (null? local)
          ""
          (list "use ("
                (ARGLIST
                  (map (lambda (l)
                         (if (eq? 'set (cadr l))
                             (string-append "&" (caddr l))
                             (caddr l)))
                       local))
                ") "))))

  (define (GLOBALS free*)
    (let ([global (filter global? free*)])
      (if (null? global)
          '()
          (list (postattach
                  (list "global "
                        (ARGLIST (map cdr global)))
                  "; ")))))

  (define (POSHANDLER pos c)
    (case pos
      [(return) (preattach "return "
                           (postattach c "; "))]
      [(sequence) (postattach c "; ")]
      [else c]))




  (define (symbol->variable sym)
    (string-append "$"
      (process-string
        (symbol->string sym))))

  (define (shadow-variable var)
    (string-append "$_"
      (list->string
        (cdr (string->list var)))))


  (define (symbol->function sym)
    (process-string
      (symbol->string sym)))

  (define (shadow-function var)
    (string-append "_"
      (if (char=? #\$ (string-ref var 0))
          (list->string
            (cdr (string->list var)))
          var)))


  (define (symbol->class symb)
    (process-string
      (symbol->string symb)))

  (define (shadow-class var)
    (string-append "_"
      (if (char=? #\$ (string-ref var 0))
          (list->string
            (cdr (string->list var)))
          var)))


  (define (process-string str)
    (process-keywords
      (process-specials str)))

  (define (process-keywords str)
    (if (memq (string->symbol str) *keywords*)
        (string-append "__" str "__")
        str))

  (define (process-specials str)
    (list->string
      (fold-right (lambda (char strls)
                    (cond
                      [(assq char *special-translation-table*)
                       => (lambda (translation)
                            (append (string->list
                                      (cadr translation))
                                    strls))]
                      [else (cons char strls)]))
                  '()
                  (string->list str))))


  (define *special-translation-table*
    '((#\+  "_plus_")
      (#\-  "_")
      (#\*  "_star_")
      (#\/  "_div_")
      (#\%  "_mod_")
      (#\<  "_lt_")
      (#\>  "_gt_")
      (#\=  "_eq_")
      (#\&  "_amp_")
      (#\!  "_b")
      (#\.  "_")
      (#\:  "__")
      (#\$  "_")
      (#\?  "_is")

      (#\space  "_")
      ))

  (define *keywords*
    '(and or true false

      do while endwhile for endfor foreach as endforeach
      break continue

      function global use return

      if else elseif endif
      switch case default endswitch
      goto

      include include_once require require_once
      array list echo print unset isset empty
      die

      new class trait interface extends implements use
      abstract private protected public static final const
      this parent
      try catch throw
      ))




  (define (specialbind n fun var)
    `(,n (function ,fun) (variable ,var)))

  (define (varbind n r)
    (cond
      [(eq? 0 (lookup-type r n))
       `(,n (variable
              ,(shadow-variable (lookup-variable r n))))]
      [else `(,n (variable
                   ,(symbol->variable n)))]))

  (define (gensymbind binder n r)
    (let ([tmpvar (gensym n)])
      (cons n (cdr (binder tmpvar r)))))

  (define (funbind n r)
    (cond
      [(lookup-function r n)
       => (lambda (var)
            `(,n (function
                   ,(shadow-function var))))]
      [else `(,n (function
                   ,(symbol->function n)))]))

  (define (classbind n r)
    (cond
      [(lookup-class r n)
       => (lambda (class)
            `(,n (class ,(shadow-class class))))]
      [else `(,n (class ,(symbol->class n)))]))


  (define (extend-frame r n*)
    (cons r (map (lambda (n)
                   (varbind n '()))
                 n*)))

  (define (extend-local r bind)
    (cons (car r)
          (cons bind
                (cdr r))))


  (define (extend-local* r n*)
    (cons (car r)
          (append
            (map (lambda (n)
                   (varbind n r))
                 n*)
            (cdr r))))

  (define (extend-local** r n*)
    (cons (car r)
          (append
            (map (cut gensymbind varbind <> r) n*)
            (cdr r))))



  (define (lookup r n proc)
    (cond
      [(null? r) #f]
      [(assq n (cdr r))
       => (lambda (ref)
            (proc (cdr ref)))]
      [else (lookup (car r) n proc)]))

  (define (lookup-variable r n)
    (lookup r n
      (lambda (ref)
        (cond
          [(assq 'variable ref)
           => (lambda (var)
                (cadr var))]
          [(assq 'function ref)
           => (lambda (func)
                (string-append "'" (cadr func) "'"))]
          [(assq 'class ref)
           => (lambda (class)
                (string-append "'" (cadr class) "'"))]
          [else (error 'lookup-variable
                       "no variable definition"
                       ref n)]))))

  (define (lookup-function r n)
    (lookup r n
      (lambda (ref)
        (cond
          [(assq 'function ref)
           => (lambda (func)
                (cadr func))]
          [(assq 'variable ref)
           => (lambda (var)
                (cadr var))]
          [else (error 'lookup-function
                       "no function definition"
                       ref n)]))))

  (define (lookup-class r n)
    (lookup r n
      (lambda (ref)
        (cond
          [(assq 'class ref)
           => (lambda (func)
                (cadr func))]
          [(assq 'variable ref)
           => (lambda (var)
                (cadr var))]
          [else (error 'lookup-class
                       "no class definition"
                       ref n)]))))


  (define (lookup-type r n)
    (let loop ([layer 0]
               [r r])
      (cond
        [(null? r) #f]
        [(assq n (cdr r))
         => (lambda (ref)
              (cond
                [(assq 'class (cdr ref)) 'class]
                [(global-env? r) 'global]
                [else layer]))]
        [else
         (loop (+ 1 layer)
               (car r))])))

  (define (global-env? r)
    (and (pair? r)
         (null? (car r))))








  (define maxwidth (make-parameter 70))
  (define indentwidth (make-parameter 2))


  (define (indent i p)
    (let ([n (- i p)])
      (when (> n 0)
        (display (make-string n #\space)))))


  (define (c-width c)
    (cond
      [(string? c) (string-length c)]
      [(null? c) 0]
      [(list? c)
       (case (car c)
         [(indent) (c-width (cdr c))]
         [(elements group) (fold-left + 0 (map c-width (cdr c)))]
         [else (fold-left + 0 (map c-width c))])]
      [else (error 'c-width "invalid compilation type" c)]))



  (define (width-fits? c p)
    (>= (maxwidth) (+ p (c-width c))))



  (define (write-c c i p)
    (cond
      [(string? c)
       (display c)
       (+ p (string-length c))]
      [(null? c) p]
      [(list? c)
       (case (car c)
         [(indent)   (write-indent (cdr c) i p)]
         [(group)    (write-group (cdr c) i p)]
         [(elements) (write-elements (cdr c) i p)]
         [else (fold-left (lambda (pp c)
                            (write-c c i pp))
                          p
                          c)])]
      [else (error 'write-c
                   "invalid compilation"
                   c)]))

  (define (write-indent c i p)
    (let ([ii (+ i (indentwidth))])
      (cond
        [(width-fits? c p)
         (when (>= i p) (indent ii p))
         (write-c c ii p)]
        [(>= i p)
         (indent ii p)
         (write-c c ii (max ii p))]
        [else
         (newline)
         (indent ii 0)
         (write-c c ii ii)])))

  (define (write-group c i p)
    (cond
      [(width-fits? c p)
       (fold-left (lambda (p c)
                    (write-c c i p))
                  p
                  c)]
      [(for-all (lambda (c)
                  (width-fits? c p))
                c)
       (for-each-c (lambda (c) (write-c c p p))
                   (lambda () (newline) (indent p 0))
                   c)]
      [(>= i p)
       (indent i p)
       (for-each-c (lambda (c) (write-c c i i))
                   (lambda () (newline) (indent i 0))
                   c)]
      [else
       (newline)
       (indent i p)
       (for-each-c (lambda (c) (write-c c i i))
                   (lambda () (newline) (indent i 0))
                   c)]))

  (define (write-elements c i p)
    (if (for-all (lambda (c)
                   (width-fits? c p))
                 c)
        (fold-left (lambda (pp c)
                     (if (width-fits? c pp)
                         (write-c c i pp)
                         (begin
                           (newline)
                           (indent p 0)
                           (write-c c i p))))
                   p
                   c)
        (let ([ii (+ i (indentwidth))])
          (newline)
          (indent ii 0)
          (fold-left (lambda (pp c)
                       (if (width-fits? c pp)
                           (write-c c ii pp)
                           (begin
                             (newline)
                             (indent ii 0)
                             (write-c c ii ii))))
                     ii
                     c))))



  (define (max* ls)
    (fold-left max 0 ls))

  (define (sum ls)
    (fold-left + 0 ls))


  (define (for-each-c proc between ls)
    (if (null? (cdr ls))
        (proc (car ls))
        (begin (proc (car ls))
               (between)
               (for-each-c proc between (cdr ls)))))

  (define (fold-left-c proc between p ls)
    (if (null? (cdr ls))
        (proc p (car ls))
        (let ([pp (proc p (car ls))])
          (between)
          (fold-left-c proc between pp (cdr ls)))))



  (define (binaryop name op)
    `(,name (function ,(binaryop-compiler op))))

  (define (operator name op)
    `(,name (function ,(operator-compiler op))))

  (define (comparator name op)
    `(,name (function ,(comparator-compiler op))))

  (define (keywordop name arity)
    `(,name (function ,(process-specials (symbol->string name)))
            (variable ,(keywordop-compiler name arity))))



  (define (binaryop-compiler op)
    (lambda (e* r pos)
      (unless (= 2 (length e*))
        (error op
               "invalid arity"
               e*))
      (receive (pre* c*) (map-compile compile e* r (position argument))
        (values pre*
          (POSHANDLER pos
            (parantheses
              (list (car c*) " " op " " (cadr c*))))))))


  (define (operator-compiler op)
    (let ([op/space (string-append " " op " ")])
      (lambda (e* r pos)
        (receive (pre* c*) (map-compile compile e* r (position argument))
          (values pre*
            (POSHANDLER pos
              (parantheses
                (list-intersperse op/space c*))))))))


  (define (comparator-compiler cmp)
    (let ([cmp/space (string-append " " cmp " ")])
      (lambda (e* r pos)
        (receive (pre* c*) (map-compile compile e* r (position argument))
          (values pre*
            (POSHANDLER pos
              (parantheses
                (list-intersperse " and "
                  (intermap 2 (lambda (a b) (list a cmp/space b))
                            c*)))))))))

  (define args
    (map string->symbol
         (map string
              (string->list
                "abcdefghijklmnopqrstuvwxyz"))))

  (define (keywordop-compiler op arity)
    (let ([arguments (take arity args)])
      (lambda (r pos)
        (compile
          `(lambda ,arguments (,op . ,arguments))
          r pos))))



  (define (array-ref e* r pos)
    (unless (= 2 (length e*))
      (error 'vector-ref
             "incorrect arity"
             e*))
    (receive (pre* c*) (map-compile compile e* r (position argument))
      (values pre*
        (POSHANDLER pos
          (list (car c*) "[" (cadr c*) "]")))))

  (define (array-set e* r pos)
    (unless (= 3 (length e*))
      (error 'vector-set!
             "incorrect arity"
             e*))
    (receive (pre* c*) (map-compile compile e* r (position argument))
      (values pre*
        (POSHANDLER pos
          (list (car c*) "[" (cadr c*) "] = " (caddr c*))))))



  (define (php-new e* r pos)
    (unless (>= 1 (length e*))
      (error 'make
             "need at least one argument (class and constructor args)"
             e*))
    (receive (pre* c*) (map-compile compile (cdr e*) r (position argument))
      (values pre*
        (POSHANDLER pos
          (list "new " (symbol->class (car e*)) " (" (ARGLIST c*) ")")))))





  (define *binary-operators*
    '((eq?      "===")
      (equal?   "==")
      ))

  (define *simple-operators*
    '(+ - * /))

  (define *operators*
    '((mod            "%")
      (string-append  ".")
      ))

  (define *comparators*
    '((=   "==")
      (<   "<")
      (>   ">")
      (<=  "<=")
      (>=  ">=")
      ))

  (define *keyword-functions*
    '((echo 1)
      (include 1)
      (require 1)
      (include-once 1)
      (require-once 1)
      (print 1)
      (unset 1)
      (isset 1)
      (empty 1)
      (die 1)
      ))

  (define *specials*
    `((vector       (function "array"))
      (vector-ref   (function ,array-ref))
      (vector-set!  (function ,array-set))

      (make         (function ,php-new))
      ))

  (define r.global
    (cons '()
          (append
            (apply append
              (map (lambda (conv data)
                     (map conv data))
                   (list
                     (lambda (simple-op)
                       (operator simple-op (symbol->string simple-op)))
                     (lambda (op)
                       (operator (car op) (cadr op)))
                     (lambda (cmp)
                       (comparator (car cmp) (cadr cmp)))
                     (lambda (binop)
                       (binaryop (car binop) (cadr binop)))
                     (lambda (special)
                       (keywordop (car special) (cadr special)))
                     )
                   (list
                     *simple-operators*
                     *operators*
                     *comparators*
                     *binary-operators*
                     *keyword-functions*
                     )))
            *specials*)))


  (define (scomp e write?)
    (receive (pre* c) (compile e (analyse e r.global) (position sequence))
      (let ([call (cons 'group (append pre* (list c)))])
        (if write?
            (write-c call 0 0)
            call))))

  )
