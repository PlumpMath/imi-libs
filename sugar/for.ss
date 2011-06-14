(library (imi sugar for)
  (export for
          for*)
  (import (rnrs)
          (imi iter iterator)
          (imi sugar for.compat))


  ;;; works similarly to (let label ([var val] ...) body ...)
  ;;;  but automatically chooses the next item of an iterator
  ;;;  and executes a given statement if an iterator has reached
  ;;;  the end
  ;;;
  ;;; (for label (<binding> ...)
  ;;;      on-end
  ;;;   body ...)
  ;;;
  ;;; <binding> : <iterator> | <user-var>
  ;;; <iterator> : [<var> in <iterator>]   ; a variable and its iterator
  ;;; <user-var> : [<var> <val>]           ; a variable and its first value
  (define-syntax for
    (lambda (stx)
      (syntax-case stx ()
        [(_ label (bindings ...)
            on-end
            body ...)
         (identifier? #'label)
         (with-syntax ([[([iterator-var iterator-gen] ...)
                         ([user-var user-val] ...)]
                        (divide-bindings #'(bindings ...))])
           (with-syntax ([(iterator ...) (generate-temporaries #'(iterator-var ...))])
             #'(let iter ([iterator iterator-gen] ...
                          [user-var user-val] ...)
                 (cond
                   [(and iterator ...)
                     (let ([iterator-var (iterator-value iterator)] ...)
                       (define (label user-var ...)
                         (iter (iterator-next iterator) ...
                               user-var ...))
                       body ...)]
                   [else on-end]))))])))


  ;;; same syntax as for, but loops through every combination
  ;;;  of the iterators
  (define-syntax for*
    (lambda (stx)
      (syntax-case stx ()
        [(_ label (bindings ...)
            on-end
            body ...)
         (identifier? #'label)
         (syntax-case (divide-bindings #'(bindings ...)) ()
           [(([iterator0-var iterator0-gen])
             ([user-var user-val] ...))
            #'(for label ([iterator0-var in iterator0-gen]
                          [user-var user-val] ...)
                   on-end
                   body ...)]
           [(([iterator0-var iterator0-gen]
              [iterator-var iterator-gen] ...)
             ([user-var user-val] ...))
            #'(for above ([iterator0-var in iterator0-gen]
                          [user-var user-val] ...)
                   on-end
                (for* label ([iterator-var in iterator-gen] ...
                             [user-var user-val] ...)
                      (above user-var ...)
                  body ...))])])))

  )
