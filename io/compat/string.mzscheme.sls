#!r6rs

(library (imi io compat string)
  (export with-input-from-string
          with-output-to-string)
  (import (only (racket)
                with-input-from-string
                with-ouput-to-string))
  )

