(library (imi io simple)
  (export call-with-i/o-ports
          with-i/o-ports
          with-input-port
          with-output-port)
  (import (rnrs)
          (imi utils parameterize)
          (imi utils void))

  ;;; calls `proc` with i/o-ports `in` `out`
  ;;;   as standard input/output ports
  ;;;
  ;;; in - input-port?
  ;;; out - output-port?
  ;;; proc - (-> pred)
  ;;;  -> pred
  (define (with-i/o-ports in out proc)
    (call-with-i/o-ports in out
      (lambda (i o)
        (parameterize ((current-input-port i)
                       (current-output-port o))
          (proc)))))

  ;;; calls `proc` with i/o-ports `in` `out`
  ;;;  as arguments and closes them afterwards
  ;;;
  ;;; in - input-port?
  ;;; out - output-port?
  ;;; proc - (-> pred input-port? output-port?)
  ;;;  -> pred
  (define (call-with-i/o-ports in out proc)
    (dynamic-wind
      void
      (lambda ()
        (proc in out))
      (lambda ()
        (close-port in)
        (close-port out))))

  ;;; calls `proc` with input-port `port`
  ;;;   as standard input-port
  ;;;
  ;;; port - input-port?
  ;;; proc - (-> pred)
  ;;;  -> pred
  (define (with-input-port port proc)
    (parameterize ((current-input-port port))
      (proc)))

  ;;; calls `proc` with output-port `port`
  ;;;   as standard output-port
  ;;;
  ;;; port - output-port?
  ;;; proc - (-> pred)
  ;;;  -> pred
  (define (with-output-port port proc)
    (parameterize ((current-output-port port))
      (proc)))

  )
