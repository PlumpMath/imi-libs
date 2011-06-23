#!r6rs

(library (imi sugar cut)
  (export cut
          <>)
  (import (rnrs)
          (for (imi sugar cut-compat)
               (meta 1)))

  ;;; short way to create a procedure which calls
  ;;;   another procedure with partial application
  ;;;   and the given arguments
  ;;;
  ;;; (cut <spec> ...)
  ;;; <spec> : <> | <identifier> | (<spec> ...)
  ;;; <identifier> : bound-identifier?
  ;;;
  ;;; examples:
  ;;; with cut         | the same as
  ;;; (cut <>)         | (lambda (proc) (proc))
  ;;; (cut + 1 <>)     | (lambda (x) (+ 1 x))
  ;;; (cut memq <> ls) | (lambda (x) (memq x ls))
  ;;; (cut <> ls <>)   | (lambda (proc n) (proc ls n))
  ;;; extension - recursive:
  ;;; (cut + 1 (/ <>)) | (lambda (x) (+ 1 (/ x)))
  (define-syntax cut
    (lambda (stx)
      (syntax-case stx (<>)
        [(cut id ...)
         (let-values ([(vars call) (transform #'(id ...))])
           (with-syntax ([(args call) (list (reverse vars)
                                            call)])
             #'(lambda args call)))])))

  )
