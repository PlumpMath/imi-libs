#!r6rs

(library (imi stream bytestream-convert)
  (export string->utf8-bytestream)
  (import (rnrs)
          (imi stream convert)
          (imi stream bytestream))

  (define (string->utf8-bytestream str)
    (list->stream
      (bytevector->u8-list
        (string->utf8 str))))

  )

