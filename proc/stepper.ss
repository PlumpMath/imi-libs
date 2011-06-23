#!r6rs

(library (imi proc stepper)
  (export make-stepper
          make-counter)
  (import (rnrs)
          (imi sugar)
          (imi math))

  ;;; DRAFTS
  ;;;   useful but ugly style

  (define (make-stepper start step)
    (let ([val (start)])
      (case-lambda 
        [()
          (let ([oldval val])
            (update! val step)
            oldval)]
        [(reset?)
          (if reset?
            (set! val (start))
            val)])))

  (define (make-counter)
    (make-stepper
      (lambda () 0)
      (lambda (x) (add1 x))))

  )
