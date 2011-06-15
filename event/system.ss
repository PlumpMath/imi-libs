(library (imi event system)
  (export event-poll
          make-listener)
  (import (rnrs)
          (imi sugar cut)
          (imi sugar rec)
          (imi sugar continuation))

  ;;; DRAFTS

  (define-record-type listener
    (fields
      mask
      proc
      (mutable data))
    (protocol
      (lambda (new)
        (lambda (mask proc . data)
          (new mask proc data)))))

  (define (event-poll poll notify? listeners)
    (let loop ()
      (let ([event (poll)])
        (when event
          (map (lambda (listener)
                 (listener-data-set! 
                   listener
                   (values->list
                     (let/cc rec
                       (apply
                         (listener-proc listener)
                         rec
                         event
                         (listener-data listener))))))
               (filter
                 (cut notify? <> event)
                 (map listener-mask listeners)))
          (loop)))))

  )
