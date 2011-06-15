(library (imi sugar syntax append)
  (export syntax-append)
  (import (rnrs)
          (imi symbol append)
          (imi sugar syntax wrapper))

  (define syntax-append
    (wrap-syntax symbol-append))

  )
