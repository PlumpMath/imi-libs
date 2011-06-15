(library (imi obj class)
  (export object)
  (import (rnrs)
          (imi obj core))

  (define object (make-object))

  (object 'set-slot! 'instance-vars (list))

  (object 'set-slot! 'constructor
    (lambda (self) self))

  (object 'set-slot! 'superclass object)

  (object 'set-slot 'subclass
    (lambda (self instance-vars)
      (let ([subclass (make-object)])
        (subclass 'set-slot 'superclass self)
        (subclass 'set-slot 'create
          (lambda (obj)
            ((self 'resolve-slot 'superclass)
             'create
             self)
            ;;;TODO - Don't exactly know what to do :D
