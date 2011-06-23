#!r6rs

(library (imi prog mpd simple-control)
  (export mpd-host
          mpd-port
          with-mpd-connection
          mpc-vol
          mpc-setvol
          mpc-play
          mpc-pause
          mpc-next
          mpc-prev
          mpc-status)
  (import (rnrs)
          (imi string processing)
          (imi utils parameter)
          (imi utils printf)
          (imi utils tester)
          (imi net io simple)
          (imi sugar thunk)
          (imi sugar cut))

  (define mpd-host (make-parameter "localhost"))
  (define mpd-port (make-parameter 6600))

  (define (read-line)
    (get-line (current-input-port)))

  (define (with-mpd-connection proc)
    (with-tcp-connection
      (mpd-host)
      (number->string (mpd-port))
      (thunk (read-line)
             (proc))))

  (define (mpc-vol v)
    (with-mpd-connection
      (thunk
        (printf "volume ~a" v)
        (newline)
        (read-line))))

  (define (mpc-setvol v)
    (with-mpd-connection
      (thunk
        (printf "setvol ~a" v)
        (newline)
        (read-line))))

  (define (mpc-play)
    (with-mpd-connection
      (thunk
        (printf "play")
        (newline)
        (read-line))))
  
  (define (mpc-pause)
    (with-mpd-connection
      (thunk
        (printf "pause")
        (newline)
        (read-line))))

  (define (mpc-next)
    (with-mpd-connection
      (thunk
        (printf "next")
        (newline)
        (read-line))))

  (define (mpc-prev)
    (with-mpd-connection
      (thunk
        (printf "previous")
        (newline)
        (read-line))))

  (define whitespace?
    (char-in " 	
"))

  (define (mpc-status)
    (with-mpd-connection
      (thunk
        (printf "status")
        (newline)
        (let loop ([info '()])
          (let ([i (read)])
            (cond
              [(eq? i 'OK) info]
              [else
                (loop
                  (cons (cons (string->symbol
                                (string-chomp 
                                  (char-in ":")
                                  (symbol->string i)))
                              (let ([val 
                                      (string-chomp whitespace?
                                                    (read-line))])
                                (cond
                                  [(eq? i 'state:) (string->symbol val)]
                                  [(or (eq? i 'time:)
                                       (eq? i 'audio:))
                                   (map string->number (string-split (char-in ":") val))]
                                  [(string->number val)]
                                  [else val])))
                        info))]))))))
  )
