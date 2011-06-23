#!r6rs

(library (imi sugar update)
  (export update!)
  (import (rnrs))

  ;;; updates a variable by assigning the return
  ;;;   value of a procedure called with the old
  ;;;   value of the variable to the variable
  ;;;
  ;;; example:
  ;;; > (define x 12)
  ;;; > (define (add1 n) (+ n 1))
  ;;; > (update! x add1)
  ;;; > x
  ;;; -> 13
  (define-syntax update!
    (lambda (x)
      (syntax-case x ()
        [(_ var updater)
          (identifier? #'var)
          #'(set! var (updater var))])))

  )
