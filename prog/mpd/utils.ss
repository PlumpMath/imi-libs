#!r6rs

(library (imi prog mpd utils)
  (export define-mpd-cmd)
  (import (rnrs)
          (imi prog mpd base))

  (define-syntax define-mpd-cmd
    (lambda (stx)
      (syntax-case stx ()
        [(def (name args ...) body ...)
         (with-syntax ([mpd-conn (datum->syntax #'def 'mpd-conn)]
                       [cmd (datum->syntax #'def 'cmd)]
                       [ret (datum->syntax #'def 'ret)])
           #'(define name
               (case-lambda
                 [(args ...)
                  (name args ... (current-mpd-connection))]
                 [(args ... mpd-conn)
                  (call-with-mpd-ports mpd-conn
                    (lambda (cmd ret)
                      body ...))])))])))

 )
