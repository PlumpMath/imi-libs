#!r6rs

(library (imi app interaction console)
  (export console-prompt-interaction
          simple-console-prompt-interaction)
  (import (rnrs)
          (imi io string))

  (define (console-prompt-interaction prompt reader)
    (lambda (glue)
      (lambda init
        (let loop ([state (apply glue init)])
          (display prompt)
          (let ([newstate (state (reader))])
            (if (procedure? newstate)
                (loop newstate)
                newstate))))))

  (define (simple-console-prompt-interaction prompt)
    (console-prompt-interaction
      prompt
      (lambda ()
        (let ([input (get-line (current-input-port))])
          (if (eof-object? input)
              (begin (newline)
                     input)
              (with-input-from-string
                (string-append "(" input "\n)")
                read))))))

  )

