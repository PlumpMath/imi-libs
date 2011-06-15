(library (imi obj simple core)
  (export make-object
          multiple-call m/c
          link-call l/c)
  (import (rnrs))

  (define (make-object)
    (let ([slots (make-eq-hashtable)])
      (define (slot-resolver self slot)
        (or (hashtable-ref slots slot #f)
            (case slot
              [(set-slot!) set-slot!]
              [(del-slot!) del-slot!]
              [(slots) get-slots]
              [(resolve-slot) slot-resolver]
              [else #f])))

      (define (set-slot! self slot proc)
        (hashtable-set! slots slot proc))

      (define (del-slot! self slot)
        (hashtable-delete! slots slot))

      (define (get-slots self)
        (hashtable-keys slots))

      (define (object slot . args)
        (apply (or ((slot-resolver object 'resolve-slot) object slot)
                   (error 'object "cannot resolve slot" slot))
               object
               args))
      
      object))
               

  (define (multiple-call obj call-args . rest)
    (cond
      [(null? rest)
        (apply obj call-args)]
      [else
        (apply obj call-args)
        (apply multiple-call obj rest)]))

  (define m/c multiple-call)

  (define (link-call obj call-args . rest)
    (cond
      [(null? rest)
        (apply obj call-args)]
      [else
        (apply link-call
               (apply obj call-args)
               rest)]))

  (define l/c link-call)

  )
