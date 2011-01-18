(library (imi io utils)
  (export read-line)
  (import (rnrs))

  ;;; reads one line from standard input or
  ;;;   an input port
  ;;;
  ;;; [port - input-port?]
  ;;;   -> string?
  (define read-line
    (case-lambda
      [() (get-line (current-input-port))]
      [(port) (get-line port)]))

  )
