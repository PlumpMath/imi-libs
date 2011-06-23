#!r6rs

(library (imi proc cursor)
  (export do-times
          cursor-doer
          cursor-doer*)
  (import (rnrs)
          (imi sugar rec)
          (imi sugar cut))

  ;;; FIXME: rewrite docs
  ;;; calls a procedure `n` times, optionally
  ;;;  with an argument, which will be passed
  ;;;  to proc and the result next time applied
  ;;;  to proc
  ;;;
  ;;; n - integer?
  ;;; proc - (-> any?)
  ;;;  -> #f
  ;;; --
  ;;; n - integer?
  ;;; proc - (-> argtype argtype)
  ;;; x - argtype
  ;;;  -> argtype
  (define (do-times n proc . cursor)
    (if (zero? n)
        (apply values cursor)
        (call-with-values
          (lambda () (apply proc cursor))
          (lambda new-cursor
            (apply do-times
                   (- n 1)
                   proc
                   new-cursor)))))


  (define (cursor-doer until? prim-proc . procs)
    #;
    (apply cursor-doer*
           (lambda (prim . rest)
             (until? prim))
           (lambda (prim . rest)
             (prim-proc prim))
           procs)

    (rec self
      (lambda (prim . args)
        (if (until? prim)
            (apply values prim args)
            (apply self
                   (prim-proc prim)
                   (map (cut <> <>)
                        procs
                        args))))))

  (define (cursor-doer* until? prim-proc . procs)
    (rec self
      (lambda (prim . args)
        (if (apply until? prim args)
            (apply values prim args)
            (apply self
                   (apply prim-proc prim args)
                   (map (cut <> <>)
                        procs
                        args))))))

  )
