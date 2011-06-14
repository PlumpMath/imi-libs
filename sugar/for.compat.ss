(library (imi sugar for.compat)
  (export divide-bindings)
  (import (rnrs))


  ;;; divides the bindings of for into
  ;;;  two groups; the bindings for an
  ;;;  iterator, which for should iterate
  ;;;  over, and the user bindings just
  ;;;  workings as in let
  ;;;
  ;;; bindings : (<binding> ...)
  ;;; binding : <iterator>
  ;;;         | <user-var>
  ;;; iterator: [<var> in <expr>]
  ;;; user-var: [<var> <expr>]
  ;;;  -> (list/c (listof/c (list/c identifier? expression?))  ; the iterators
  ;;;             (listof/c (list/c identifier? expression?))) ; the user vars
  (define (divide-bindings bindings)
    (let loop ([bindings bindings]
               [iterators '()]
               [user '()])
      (if (null? bindings)
        (list (reverse iterators)
              (reverse user))
        (syntax-case (car bindings) (in)
          [(iterator in iterator-gen)
           (loop (cdr bindings)
                 (cons #'(iterator iterator-gen)
                       iterators)
                 user)]
          [(user-var user-val)
           (loop (cdr bindings)
                 iterators
                 (cons #'(user-var user-val)
                       user))]))))

  )
