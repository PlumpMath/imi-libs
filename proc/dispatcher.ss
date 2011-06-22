(library (imi proc dispatcher)
  (export dispatchq
          dispatchv
          dispatch
          dispatchp)
  (import (rnrs))

  (define (generic-dispatcher name choose)
    (lambda (criteria ls . curried-args)
      (apply (cdr (or (choose criteria ls)
                      (error name
                             "could not dispatch"
                             criteria ls)))
             curried-args)))

  (define dispatchq (generic-dispatcher 'dispatchq assq))
  (define dispatchv (generic-dispatcher 'dispatchv assv))
  (define dispatch (generic-dispatcher 'dispatch assoc))
  (define dispatchp (generic-dispatcher 'dispatchp assp))

  )
