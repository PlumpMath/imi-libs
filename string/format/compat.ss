#!r6rs

(library (imi string format compat)
  (export format)
  (import (rnrs)
          (imi io simple))

  (define (format fmtstring . args)
    (with-string-i/o fmtstring
      (lambda ()
        (let loop ([args args])
          (define (with-arg proc)
            (proc (car args))
            (loop (cdr args)))
          (define (next)
            (loop args))

          (case (peek-char)
            [(#\~)
              (read-char)
              (case (peek-char)
                [(#\s) 
                  (read-char)
                  (with-arg write)]
                [(#)
                  (read-char)
                  (with-arg display)]
                [(#\d)
                  (read-char)
                  (with-arg
                    (lambda (arg)
                      (if (number? arg)
                        (display arg)
                        (error 'format "not a number" arg))))]
                [(#\~)
                  (read-char)
                  (write-char #\~)
                  (next)]
                [(#\%)
                  (read-char)
                  (newline)
                  (next)]
                [else
                  (error 'format "cannot handle escape" (read-char))])]
            [else
              (write-char (read-char))
              (next)])))))

  )
