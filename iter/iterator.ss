(library (imi iter iterator)
  (export iterate
          make-iterator
          iterator?
          iterator-value
          iterator-next)
  (import (rnrs))

  ;;; holds the actual value and a
  ;;;   procedure which returns the
  ;;;   next iterator (similar to conses
  ;;;   in lists, same concept as streams)
  (define-record-type iterator
    (fields value proc-next))

  ;;; get the next iterator - calls
  ;;;   the procedure of iterator to
  ;;;   get the next iterator
  ;;;
  ;;; iterator - iterator?
  ;;;  -> iterator?
  (define (iterator-next iterator)
    ((iterator-proc-next iterator)))

  ;;; generates an iterator with the
  ;;;   actual value and a procedure
  ;;;   holding the second expression
  ;;;   to get the next iterator
  ;;;
  ;;; (iterate value next)
  (define-syntax iterate
    (lambda (stx)
      (syntax-case stx ()
        [(_ val next)
         #'(make-iterator
             val
             (lambda () next))])))

  )
