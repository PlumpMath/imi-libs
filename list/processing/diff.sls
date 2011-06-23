#!r6rs

(library (imi list processing diff)
  (export diff)
  (import (rnrs)
          (imi sugar receive)
          (imi list processing largest-common-sequence))

  (define (diff eq? orig changed)
    (receive (sublen subsequence) (lcs eq orig changed)
      (let ([eq? (lambda (a b)
                   (eq? (car a)
                        (car b)))])
        (let loop ([orig orig]
                   [chng changed]
                   [subs subsequence])
          (cond
            [(null? subs)
             (cond [(not (null? orig))
                    (cons (deletion (car orig))
                          (loop (cdr orig)
                                chng
                                subs))]
                   [(not (null? subs))
                    (cons (insertion (car chng))
                          (loop orig
                                (cdr chng)
                                subs))]
                   [else '()])]
            [(not (eq? orig subs))
             (cons (deletion (car orig))
                   (loop (cdr orig)
                         chng
                         subs))]
            [(not (eq? chng subs))
             (cons (insertion (car chng))
                   (loop orig
                         (cdr chng)
                         subs))]
            [else
             (cons (original (car orig))
                   (loop (cdr orig)
                         (cdr chng)
                         (cdr subs)))])))))


  (define (original sth)  `(0 ,sth))
  (define (insertion sth) `(+ ,sth))
  (define (deletion sth)  `(- ,sth))

  )

