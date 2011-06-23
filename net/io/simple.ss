#!r6rs

(library (imi net io simple)
  (export as-tcp-server
          call-as-tcp-server
          call-with-accepted-connection
          call-with-tcp-server-socket
          #;as-tcp-server-nonblocking
          with-tcp-connection
          call-with-tcp-connection
          #;with-tcp-connection-nonblocking)
  (import (rnrs)
          (imi io simple)
          (imi utils void)
          (imi net io tcp))

  (define (as-tcp-server port-number proc)
    (call-as-tcp-server port-number
      (lambda (i o quit)
        (with-i/o-ports i o
          (lambda () (proc quit))))))

  (define (call-as-tcp-server port-number proc)
    (call/cc
      (lambda (quit)
        (call-with-tcp-server-socket port-number
          (lambda (server)
            (do () (#f) ;infinite loop
              (call-with-accepted-connection server 
                (lambda (i o) (proc i o quit)))))))))

  (define (standard-transcoded port)
    (transcoded-port port (native-transcoder)))

  (define (call-with-accepted-connection server proc)
    (call-with-values
      (lambda () (accept-connection server))
      (lambda (i o)
        (call-with-i/o-ports (standard-transcoded i)
                             (standard-transcoded o)
                             proc))))

  (define (call-with-tcp-server-socket port-number proc)
    (let ((server (tcp-server-socket port-number)))
      (dynamic-wind
        void
        (lambda ()
          (proc server))
        (lambda ()
          (close-tcp-server-socket server)))))

  (define (with-tcp-connection host service proc)
    (call-with-tcp-connection host service
      (lambda (i o)
        (with-i/o-ports i o proc))))

  (define (call-with-tcp-connection host service proc)
    (call-with-values
      (lambda () (tcp-connect host service))
      (lambda (i o)
        (call-with-i/o-ports (standard-transcoded i)
                             (standard-transcoded o)
                             proc))))

  )
