#!r6rs

(library (imi text processing)
  (export split-lines)
  (import (rnrs)
          (imi string processing))


  (define (newline? sth)
    (char=? sth #\newline))

  (define (split-lines str)
    (string-split newline? str))

  )

