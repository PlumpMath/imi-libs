#!r6rs

(library (imi sugar and-let)
  (export and-let)
  (import (rnrs))

  ;;; binds a value to an identifier and
  ;;;   goes on if the result is true (not #f);
  ;;;   the identifier will be visible in subsequent
  ;;;   binds and the last expression; the last
  ;;;   expression is a simple expression;
  ;;;   similar syntax to let*
  ;;;
  ;;; (and-let [identifier expression] ... last-expression)
  (define-syntax and-let
    (lambda (stx)
      (syntax-case stx ()
        [(and-let expr)
         #'expr]
        [(and-let (id expr) . rest)
         #'(let ([id expr])
             (and id
                  (and-let . rest)))]
        [(and-let (expr) . rest)
         #'(and expr
                (and-let . rest))])))

  )
