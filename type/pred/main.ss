#!r6rs

(library (imi type pred)
  (export pred
          syntax-pred
          
          check-pred)
  (import (rnrs)
          (imi sugar case-pred))

  ;;; FIXME: there has nothing been fixed by this syntax;
  ;;;        the predicates itself have to be syntax generating
  ;;;        procedures

  ;;; to create a predicate function, works
  ;;;  similarly to lambda, but creates a
  ;;;  procedure which creates a procedure
  ;;;  with one argument (call-arg); it is
  ;;;  realized as syntax so that an argument
  ;;;  to pred is not really an argument and
  ;;;  resolved on call, but can recursively
  ;;;  refer to itself and only be resolved
  ;;;  in the function
  ;;;
  ;;; (pred call-arg <pred-args> body ...)
  ;;;   <pred-args> : restarg
  ;;;               | (args ...)
  ;;;
  ;;;  -> (->* (-> any? any?) any?)
  (define-syntax pred
    (lambda (stx)
      (syntax-case stx ()
        [(_ call-arg pred-args body ...)
         #'(lambda pred-args
             (lambda (call-arg)
               body ...))])))

  ;;; to create a predicate syntax transformer,
  ;;;  similarly to pred, but pred-args are syntax-case
  ;;;  "arguments"; in general this is only a
  ;;;  short way of using (lambda (stx) (syntax-case stx () ...))
  ;;;
  ;;; (pred call-arg <pred-args> body)
  ;;;   <pred-args> - syntax-case match-rule
  ;;;
  ;;;  -> (-> syntax? syntax?)
  (define-syntax syntax-pred
    (lambda (stx)
      (syntax-case stx ()
        [(_ call-arg pred-args body)
         #'(lambda (stx)
             (syntax-case stx ()
               [(_ . pred-args)
                #'body]))])))


  ;;; checks if `obj` applies to `pred`
  ;;;   `pred` can either be a procedure
  ;;;   to check if `obj` is the desired
  ;;;   value or another value itself, which
  ;;;   is then compared by eqv? with `obj`
  ;;;
  ;;; pred - (or/c (-> any? any?) (neg procedure?))
  ;;; obj - any?
  ;;;  -> any?
  (define (check-pred pred obj)
    (case-pred pred
      [(procedure?) (pred obj)]
      [else (eqv? pred obj)]))

  )
