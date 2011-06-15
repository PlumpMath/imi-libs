(library (imi list construct)
  (export construct-by
          maxlen-constructor
          range)
  (import (rnrs)
          (rnrs mutable-pairs)
          (imi utils gensym)
          (imi math))

  ;;; FIXME: check if really needed and
  ;;;        maybe rewrite in nicer style

  (define (construct-by proc)
    (let ([ls (cons '() '())])
      (let loop ([end ls]
                 [pos 0])
        (cond
          [(proc pos)
            => (lambda (new)
                 (set-cdr! end (list new))
                 (loop (cdr end) (add1 pos)))]
          [else
            (cdr ls)]))))

  (define (maxlen-constructor n proc)
    (lambda (x) (and (< x n) (proc x))))

  (define (range n)
    (construct-by (maxlen-constructor n values)))

  )
