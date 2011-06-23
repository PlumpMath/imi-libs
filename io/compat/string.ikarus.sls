#!r6rs

(library (imi io compat string)
  (export with-input-from-string
          with-output-to-string)
  (import (only (ikarus)
                with-input-from-string
                with-output-to-string)))
