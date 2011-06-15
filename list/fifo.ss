(library (imi list fifo)
  (export make-fifo
          fifo-length
          fifo-empty?
          fifo-queue!
          fifo-pop!)
  (import (rnrs)
          (rnrs mutable-pairs)
          (imi utils records updater)

  (define-record-type fifo
    (fields
      [contents mutable]
      [end mutable])
    (protocol
      (lambda (new)
        (lambda ()
          (new '() '())))))

  (define fifo-contents-update!
    (updater fifo-contents-set! fifo-contents))

  (define fifo-end-update!
    (updater fifo-end-set! fifo-end))

  (define (fifo-length fifo)
    (length (fifo-contents fifo)))

  (define (fifo-empty? fifo)
    (null? (fifo-contents fifo)))

  (define (fifo-queue! fifo elem)
    (let ([cont (list elem)])
      (cond
        [(fifo-empty? fifo)
         (fifo-contents-set! fifo cont)
         (fifo-end-set! fifo cont)]
        [else
         (fifo-end-update! fifo
           (lambda (end)
             (set-cdr! end cont)
             cont))])))

  (define (fifo-pop! fifo)
    (cond
      [(fifo-empty? fifo)
       (error 'fifo-pop! "fifo is empty" fifo)]
      [else
       (let ([elem (car (fifo-contents fifo))])
         (fifo-contents-update! fifo cdr)
         elem)]))

  )
