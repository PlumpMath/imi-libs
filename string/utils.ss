#!r6rs

(library (imi string utils)
  (export string-last-pos
          whitespace?
          
          string-pad
          string-group)
  (import (rnrs)
          (imi math)
          (imi list utils)
          (imi utils tester))
  
  ;;; position of last character in `str`
  ;;;
  ;;; str - string?
  ;;;  -> integer?
  (define (string-last-pos str)
    (sub1 (string-length str)))

  ;;; checks if a character is a whitespace
  (define whitespace?
    (char-in " \t\r\n"))

  
  (define (string-pad len fill str)
    (list->string
      (list-pad len fill (string->list str))))

  (define (string-group len str)
    (map list->string
         (list-group len (string->list str))))


  )
