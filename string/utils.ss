(library (imi string utils)
  (export string-last-pos
          whitespace?)
  (import (rnrs)
          (imi math)
          (imi utils check))
  
  ;;; position of last character in `str`
  ;;;
  ;;; str - string?
  ;;;  -> integer?
  (define (string-last-pos str)
    (sub1 (string-length str)))

  ;;; checks if a character is a whitespace
  (define whitespace?
    (char-in " \t\r\n"))

  )
