(library (imi utils records updater)
  (export updater)
  (import (rnrs))

  (define (updater setter getter)
    (lambda (data proc)
      (setter data
              (proc (getter data)))))

  )
