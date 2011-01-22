(library (imi sugar cut)
  (export cut)
  (import (rnrs))

  ;;; short way to create a procedure which calls
  ;;;   another procedure with partial application
  ;;;   and the given arguments
  ;;;
  ;;; (cut <spec> ...)
  ;;; <spec> : <> | <identifier>
  ;;; <identifier> : bound-identifier?
  ;;;
  ;;; examples:
  ;;; with cut         | the same as
  ;;; (cut <>)         | (lambda (proc) (proc))
  ;;; (cut + 1 <>)     | (lambda (x) (+ 1 x))
  ;;; (cut memq <> ls) | (lambda (x) (memq x ls))
  ;;; (cut <> ls <>)   | (lambda (proc n) (proc ls n))
  (define-syntax cut
    (syntax-rules (<>)
      [(cut id ...)
         (internal-cut (id ...) () ())]))

  (define-syntax internal-cut
    (syntax-rules (<>)
      [(_ (<> rest ...) (slots ...) (args ...))
         (internal-cut (rest ...) (slots ... x) (args ... x))]
      [(_ (expr rest ...) (slots ...) (args ...))
         (internal-cut (rest ...) (slots ...) (args ... expr))]
     [(_ () (slots ...) (proc args ...))
         (lambda (slots ...)
           (proc args ...))]))

  )
