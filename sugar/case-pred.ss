#!r6rs

(library (imi sugar case-pred)
  (export case-pred)
  (import (rnrs)
          (imi sugar cut))

  ;;; chooses case by applying a predicate to
  ;;;   a given value; same syntax as case
  ;;;
  ;;; (case-pred value <case> ...) ; value will be only evaluated once as written
  ;;;
  ;;; <case> : [<preds> expr ...]  ; if value applies to one pred evaluate expr ...
  ;;;        | [<preds> => proc]   ; if value applies to one pred call proc with
  ;;;                              ;   the return value of the pred
  ;;;        | [else expr ...]     ; in every other case evaluate expr ...
  ;;;
  ;;; <preds> : (predicate ...)    ; the predicates
  (define-syntax case-pred
    (lambda (stx)
      (syntax-case stx (else =>)
        [(case-pred expr actions ...)
         (not (identifier? #'expr))
         #'(let ([t expr])
             (case-pred t actions ...))]
        [(case-pred t
           [(preds ...) expr ...]
           rest ...)
         #'(if (or (preds t) ...)
             (begin expr ...)
             (case-pred t rest ...))]
        [(case-pred t
           [(preds ...) => proc]
           rest ...)
         #'(let ([x (or (preds t) ...)])
             (if x
               (proc x)
               (case-pred t rest ...)))]
        [(case-pred t
           [else expr ...])
         #'(begin expr ...)]
        [(case-pred t)
         #'(begin)])))

  )
