#!r6rs

(library (imi utils print)
  (export print
          print-to
          print-delimiter)
  (import (rnrs)
          (imi list utils)
          (imi utils void)
          (imi utils parameter))

  (define print-delimiter (make-parameter " "))

  (define print/delimiter
    (case-lambda
      [(x) (display x)
           (display (print-delimiter))]
      [(x port) (display x port)
                (display (print-delimiter)
                         port)]))

  (define (print . ls)
    (let loop ([ls ls])
      (cond
        [(null? ls) (newline)]
        [(length=? ls 1)
         (display (car ls))
         (loop (cdr ls))]
        [else
         (print/delimiter (car ls))
         (loop (cdr ls))])))

  (define (print-to port . ls)
    (let loop ([ls ls])
      (cond
        [(null? ls) (newline port)]
        [(length=? ls 1)
         (display (car ls)
                  port)
         (loop (cdr ls))]
        [else
         (print/delimiter (car ls)
                          port)
         (loop (cdr ls))])))

  )
