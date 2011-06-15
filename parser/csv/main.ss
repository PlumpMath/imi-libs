(library (imi parser csv)
  (export read-csv
          csv-delimiter)
  (import (rnrs)
          (imi proc construct)
          (imi proc predicate logic)
          (imi sugar cut)
          (imi utils tester)
          (imi utils parameter))

  ;;; reads a csv (comma seperated values) line
  ;;;  from standard input, the delimiters are
  ;;;  defined by csv-delimiter, additionally
  ;;;  the newline character
  ;;;
  ;;; -> (listof/c string?)
  (define (read-csv)
    (construct-list*
      read-csv-token
      (lambda (token)
        (not (or (eof-object? (peek-char))
                 (char=? #\newline (peek-char)))))))

  ;;; the delimiters for the values, standard
  ;;;  is comma and semicolon
  ;;;
  ;;; (parameter/c (listof/c char?))
  (define csv-delimiter
    (make-parameter
      '(#\, #\;)))

  ;;; reads an token of a csv, that is the
  ;;;  text until an delimiter, except it
  ;;;  is in an string; the delimiter is
  ;;;  a newline or the characters given
  ;;;  in the csv-delimiter parameter
  ;;;
  ;;; -> string?
  (define (read-csv-token)
    (if (char=? (peek-char)
                #\")
        (read)
        (list->string
          (construct-list*
            (cut read-char)
            (or/p eof-object?
                  (cut char=? <> #\newline)
                  (tester exists
                          char=?
                          (csv-delimiter)))))))

  )
