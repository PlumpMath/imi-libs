(library (imi obj simple proto)
  (export object)
  (import (rnrs)
          (imi obj core)
          (imi sugar rec))

  (define object (make-object))

  (object 'set-slot! 'add-var!
    (rec add-var!
      (case-lambda 
        [(self slot val) 
         (add-var! self slot val
                   (lambda (new-val) new-val))]
        [(self slot val proc)
         (self 'set-slot! slot
           (case-lambda
             [(self) val]
             [(self new-val)
              (set! val (proc new-val))]))])))

  (object 'set-slot! 'update-slot!
    (lambda (self slot proc)
      (self 'set-slot!
            (proc (self 'resolve-slot slot)))))

  (object 'set-slot! 'initialize
    (lambda (self) self))

  (object 'set-slot! 'proto
    (lambda (self) object))

  (object 'set-slot! 'super
    (lambda (self slot . args)
      (apply (or ((self 'proto) 'resolve-slot slot)
                 (error self "cannot find slot in prototypes" slot))
             self
             args)))

  (object 'set-slot! 'new
    (lambda (super . args)
      (let ([obj (make-object)])
        (obj 'set-slot! 'proto 
           (lambda (self) super))
        (let ([resolve (obj 'resolve-slot 'resolve-slot)])
          (obj 'set-slot! 'resolve-slot
            (lambda (self slot)
              (or (resolve self slot)
                  ((self 'proto) 'resolve-slot slot)))))
        (lambda args
          (apply obj 'super 'initialize args)
          obj))))

  )
