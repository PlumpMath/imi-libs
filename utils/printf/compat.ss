(library (imi utils printf compat)
  (export printf)
  (import (rnrs)
          (imi string format))

  (define (printf form . args)
    (display (apply format form args)))

  )
