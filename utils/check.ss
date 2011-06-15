(library (imi utils check)
  (export check)
  (import (rnrs))

  (define (check pred obj)
    (and (pred obj)
         obj))

  )
