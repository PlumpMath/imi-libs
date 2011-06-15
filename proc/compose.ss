(library (imi proc compose)
  (export compose
          preproc
          pre
          neg)
  (import (rnrs)
          (imi sugar cut))

  ;;; generate a procedure which processes
  ;;;   all its arguments with `pre` and
  ;;;   calls `proc` with them
  ;;;
  ;;; pre - (-> between in)
  ;;; proc - (->* out (listof/c between))
  ;;;  -> (->* out (listof/c in))
  (define (preproc pre proc)
    (lambda x
      (apply proc (map pre x))))

  ;;; same as preproc but arguments in
  ;;;   reverse order (maybe simpler to
  ;;;   handle)
  ;;;
  ;;; proc - (->* out (listof/c between))
  ;;; preproc - (-> between in)
  ;;;  -> (->* out (listof/c in))
  (define (pre proc preproc)
    (lambda args
      (apply proc (map preproc args))))

  ;;; generates a procedure composed of
  ;;;   `outer` and `inner`, meaning
  ;;;   the arguments will be passed to
  ;;;   `inner`, returnvalues passed as
  ;;;   arguments to `outer`, returning
  ;;;   its returnvalues
  ;;;
  ;;; outer - (->/values out mid)
  ;;; inner - (->* mid in)
  ;;;  -> (->* out in)
  (define (compose outer inner)
    (lambda x
      (call-with-values
        (cut apply inner x)
        outer)))

  ;;; negates a procedure, meaning it
  ;;;   generates a procedure which will
  ;;;   return the opposite of `proc`
  ;;;
  ;;; proc - (->* any? in)
  ;;;  -> (->* boolean? in)
  (define (neg proc)
    (compose not proc))

  )
