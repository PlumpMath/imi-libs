#!r6rs

(library (imi prog mpd base)
  (export with-mpd-connection
          call-with-mpd-ports

          current-mpd-connection

          mpd-connect
          mpd-disconnect
          mpd-command-port
          mpd-return-port

          handle-mpd-return
          
          &mpd-condition
          make-mpd-condition
          condition-mpd-connection
          mpd-condition?
          
          &mpd-code-condition
          make-mpd-code-condition
          condition-mpd-code
          condition-mpd-cmd-no
          mpd-code-condition?)
  (import (rnrs)
          (imi io extra)
          (imi net io tcp)
          (imi list processing)
          (imi string processing)
          (imi string utils)
          (imi sugar begin0)
          (imi utils check)
          (imi utils print)
          (imi utils parameter)
          (imi utils parameterize))

  ;;; DRAFT
  ;;;   good - similar definitions to r6rs

  (define current-mpd-connection (make-parameter #f))


  (define-record-type mpd-connection
    (fields (immutable command-port mpd-command-port)
            (immutable return-port mpd-return-port)))

  (define (mpd-connect host port)
    (let-values ([(in out)
                  (tcp-connect (or host "localhost")
                               (number->string (or port 6600)))])
      (make-mpd-connection
        (transcoded-port out (make-transcoder (utf-8-codec)))
        (transcoded-port in (make-transcoder (utf-8-codec))))))

  (define (mpd-disconnect mpd-connection)
    (print-to (mpd-command-port mpd-connection)
              "close")
    (begin0
      (read-line (mpd-return-port mpd-connection))
      (close-port (mpd-command-port mpd-connection))
      (close-port (mpd-return-port mpd-connection))))


  (define (with-mpd-connection host port thunk)
    (parameterize ([current-mpd-connection (mpd-connect host port)])
      (read-line)
      (thunk)
      (mpd-disconnect (current-mpd-connection))))

  (define (call-with-mpd-ports mpd-connection proc)
    (proc
      (mpd-command-port mpd-connection)
      (mpd-return-port mpd-connection)))



  (define-condition-type &mpd-condition &condition
    make-mpd-condition mpd-condition?
    (mpd-connection condition-mpd-connection))

  (define-condition-type &mpd-code-condition &mpd-condition
    make-mpd-code-condition mpd-code-condition?
    (mpd-code condition-mpd-code)
    (mpd-cmd-no condition-mpd-cmd-no))

  (define (handle-mpd-return mpd-conn line)
    (let ([tokens (string-split whitespace? line)])
      (cond
        [(string=? (car tokens) "OK")]
        [(string=? (car tokens) "ACK")
         (let ([code/cmp-no
                 (string-split (char-in "@")
                               (string-chomp (char-in "[]")
                                             (cadr tokens)))])
           (raise
             (condition
               (make-mpd-condition
                 mpd-conn)
               (make-mpd-code-condition
                 (car code/cmp-no)
                 (cadr code/cmp-no))
               (make-who-condition
                 (string-chomp (char-in "{}") (caddr tokens)))
               (make-message-condition
                 (apply string-append
                        (list-intersperse " " (cdddr tokens)))))))]
        [else
          (raise
            (condition
              (make-error)
              (make-mpd-condition
                mpd-conn)
              (make-message-condition
                "unexpected answer")
              (make-irritants-condition
                (list line))))])))

  )
